module StructuralEditor.ListEditor where

import Array
import Random
import Dict exposing (Dict)
import Signal exposing (Address)
import Json.Encode as Encode
import Task exposing (Task)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Keyboard exposing (KeyCode)
import Keyboard.Keys exposing (..)
import ElmFire exposing (Priority(..), Location)
import Component exposing (Update)
import TaskUtil
import StructuralEditor.Editor as Editor exposing (Editor)
import StructuralEditor.Combobox as Combobox

-- TODO fix lost focus when deleting last item
-- TODO move to next/previous element when pressing enter or right/left
-- TODO implement move
-- TODO implement priority fixing

type alias ListEditor item =
  Editor (Model item)

type alias Model item =
  {
    items: Dict String (Item item),
    inserter: Editor item,
    lastInserted: Maybe Priority
  }

type alias Item item =
  {
    priority: Priority,
    editor: Editor item
  }

type Action action =
  None |
  ItemAction {
    url: String,
    action: Editor.Action action
  } |
  Insert Position |
  Delete String |
  InserterAction (Editor.Action action)

type Position =
  Before String |
  After String

getItemAction : String -> Editor.Action action -> Action action
getItemAction url action =
  ItemAction {
    url =
      url,
    action =
      action
  }

init : Context item action -> Location -> Address (Editor.Action (Action action)) -> Update (ListEditor item)
init context location address =
  Editor.init
    (initialModel context location (inserterAddress address))
    location
    address

inserterAddress : Address (Editor.Action (Action action)) -> Address (Editor.Action action)
inserterAddress address =
  address `Signal.forwardTo` (InserterAction >> Editor.CustomAction)

initialModel : Context item action -> Location -> Address (Editor.Action action) -> Model item
initialModel context location itemAddress =
  {
    items =
      Dict.empty,
    inserter =
      Editor.init
        context.initialItem
        (location |> ElmFire.push)
        itemAddress
      |> .model,
    lastInserted =
      Nothing
  }

type alias Context item action =
  {
    initialItem: item,
    itemUpdateContext: Editor.UpdateContext (Editor item) action,
    separator: Separator,
    itemViewContext: Editor.ViewContext item action
  }

type alias Separator =
  {
    html: Html,
    keyCode: KeyCode
  }

update : Context item action -> Address (Editor.Action (Action action)) -> Editor.Action (Action action) -> ListEditor item -> Update (ListEditor item)
update context address action editor =
  Editor.update (updateContext context editor.location) address action editor

updateContext : Context item action -> Location -> Editor.UpdateContext (ListEditor item) (Action action)
updateContext context location =
  {
    valueChanged _ _ editor =
      Component.return editor,
    childAdded listAddress snapshot editor =
      let result =
            Component.returnAndRun
              { editor | model <- updatedModel }
              itemUpdate.task
          updatedModel =
            { model | items <- model.items |> Dict.insert url item }
          model =
            editor.model
          url =
            snapshot.reference |> ElmFire.toUrl
          item =
            {
              priority =
                snapshot.priority,
              editor =
                itemUpdate.model
            }
          itemUpdate =
            Editor.init
              context.initialItem
              (snapshot.reference |> ElmFire.location)
              itemAddress
          itemAddress =
            listAddress `Signal.forwardTo` (getItemAction url)
      in result,
    childRemoved listAddress snapshot editor =
      let result =
            editor.model.items |> Dict.get url |> Maybe.map (\item ->
              Component.returnAndRun
                { editor | model <- updatedModel }
                (item.editor |> unsubscribe |> TaskUtil.swallowError "ElmFire.unsubscribe failed")
            ) |> Maybe.withDefault (Component.return editor)
          updatedModel =
            { model | items <- model.items |> Dict.remove url }
          model =
            editor.model
          url =
            snapshot.reference |> ElmFire.toUrl
          itemAddress =
            listAddress `Signal.forwardTo` (getItemAction url)
      in result,
    childMoved _ snapshot editor =
      let result =
            Component.return
              { editor | model <- updatedModel }
          updatedModel =
            { model | items <-
              model.items |> Dict.update url (Maybe.map (\item ->
                { item | priority <- snapshot.priority }
              ))
            }
          model =
            editor.model
          url =
            snapshot.reference |> ElmFire.toUrl
      in result,
    customAction listAddress action editor =
      case action of
        None ->
          Component.return editor
        ItemAction itemAction ->
          editor.model.items |> Dict.get itemAction.url |> Maybe.map (\item ->
            let result =
                  Component.returnAndRun
                    { editor | model <- updatedModel }
                    itemUpdate.task
                updatedModel =
                  { model | items <- updatedItems }
                model =
                  editor.model
                updatedItems =
                  model.items |> Dict.insert itemAction.url updatedItem
                updatedItem =
                  { item | editor <- itemUpdate.model }
                itemUpdate =
                  Editor.update
                    context.itemUpdateContext
                    itemAddress
                    itemAction.action
                    item.editor
                itemAddress =
                  listAddress `Signal.forwardTo` (getItemAction itemAction.url)
            in result
          ) |> Maybe.withDefault (Component.return editor)
        Delete url ->
          Component.returnAndRun
            editor
            (
              url
              |> ElmFire.fromUrl
              |> ElmFire.remove
              |> TaskUtil.swallowError "Failed to delete item"
            )
        Insert position ->
          let result =
                Component.returnAndRun
                  { editor | model <- updatedModel }
                  task
              task =
                location
                |> ElmFire.push
                |> ElmFire.setWithPriority (Encode.string "") priority
                |> TaskUtil.swallowError "Failed to insert item"
              updatedModel =
                { model | lastInserted <- Just priority }
              model =
                editor.model
              priority =
                position |> getPriority model
          in result
        InserterAction inserterAction ->
          let result =
                Component.returnAndRun
                  { editor | model <- updatedModel }
                  inserterUpdate.task
              updatedModel =
                  { model | inserter <- inserterUpdate.model }
              model =
                editor.model
              inserterUpdate =
                { inserter | priority <- NumberPriority 0 }
                |> Editor.update context.itemUpdateContext inserterAddress inserterAction
              inserter =
                model.inserter
              inserterAddress =
                listAddress `Signal.forwardTo` InserterAction
          in result
  }

unsubscribe : Editor model -> Task ElmFire.Error ()
unsubscribe editor =
  editor.subscriptions
  |> Dict.values
  |> List.map (\result ->
    result
    |> Result.toMaybe
    |> Maybe.map ElmFire.unsubscribe
    |> Maybe.withDefault (Task.succeed ())
  )
  |> TaskUtil.parallel

getPriority : Model item -> Position -> Priority
getPriority model position =
  let result =
        ((getPriorityAt indexBefore) + (getPriorityAt indexAfter)) / 2 |> NumberPriority
      getPriorityAt index =
        items |> Array.fromList |> Array.get index |> Maybe.map (\(_, item) ->
          case item.priority of
            NumberPriority priority ->
              priority
            _ ->
              0.0
        ) |> Maybe.withDefault ((if index < 0 then Random.minInt else Random.maxInt) |> toFloat)
      indicesByUrl =
        items |> List.indexedMap (\index (url, _) ->
          (url, index)
        ) |> Dict.fromList
      items =
        model |> get
      indexBefore =
        case position of
          After urlBefore ->
            indicesByUrl |> Dict.get urlBefore |> Maybe.withDefault Random.minInt
          Before urlAfter ->
            indicesByUrl |> Dict.get urlAfter |> Maybe.map (\otherIndex -> otherIndex - 1) |> Maybe.withDefault Random.minInt
      indexAfter =
        case position of
          Before urlAfter ->
            indicesByUrl |> Dict.get urlAfter |> Maybe.withDefault Random.maxInt
          After urlBefore ->
            indicesByUrl |> Dict.get urlBefore |> Maybe.map (\otherIndex -> otherIndex + 1) |> Maybe.withDefault Random.maxInt
  in result

view : Context item action -> Bool -> Address (Editor.Action (Action action)) -> ListEditor item -> Html
view context focused address editor =
  Editor.view (viewContext context) focused address editor

viewContext : Context item action -> Editor.ViewContext (Model item) (Action action)
viewContext context =
  {
    view focused listAddress editor =
      let result =
            Html.div
              []
              itemViews
          itemViews =
            if items |> List.isEmpty then
              [viewInserter context listAddress editor]
            else
              items
              |> List.map (\(url, item) ->
                [
                  viewTransformer context.separator False listAddress editor url,
                  Editor.view context.itemViewContext (editor.model.lastInserted == Just item.priority) (itemAddress url) item.editor,
                  viewTransformer context.separator True listAddress editor url
                ]
              )
              |> List.intersperse [context.separator.html]
              |> List.concat
          items =
            editor.model |> get
          itemAddress url =
            listAddress `Signal.forwardTo` (getItemAction url)
      in result
  }

viewInserter : Context item action -> Address (Action action) -> ListEditor item -> Html
viewInserter context listAddress editor =
  Editor.view context.itemViewContext False (listAddress `Signal.forwardTo` InserterAction) editor.model.inserter

viewTransformer : Separator -> Bool -> Address (Action action) -> ListEditor item -> String -> Html
viewTransformer separator after address editor url =
  let result =
        Html.span
          [
            Attributes.contenteditable True,
            Attributes.style [
              ("padding", "0.1em")
            ],
            Combobox.handleKeys True [removerKey.keyCode, tab.keyCode],
            Events.onKeyUp address keyUpAction
          ]
          []
      keyUpAction keyCode =
        if keyCode == removerKey.keyCode then
          Delete url
        else if keyCode == separator.keyCode then
          Insert (if after then After url else Before url)
        else
          None
      removerKey =
        if after then backspace else delete
      inserterPosition =
        if after then After url else Before url
  in result

{-- Returns the list of items along with their URLs ordered by priority.
-}
get : Model data -> List (String, Item data)
get model =
  model.items
  |> Dict.toList
  |> List.sortBy internalPriority

internalPriority : (String, Item data) -> (Int, Float, String, String)
internalPriority (url, item) =
  case item.priority of
    NoPriority ->
      (0, 0, "", url)
    NumberPriority priority ->
      (1, priority, "", url)
    StringPriority priority ->
      (2, 0, priority, url)
