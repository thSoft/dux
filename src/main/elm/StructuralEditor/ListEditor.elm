module StructuralEditor.ListEditor where

import Array
import Random
import Dict exposing (Dict)
import Signal exposing (Address)
import Task exposing (Task)
import Task.Extra
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

type alias ListEditor item =
  Editor (Model item)

type alias Model item =
  {
    items: Dict String (Item item),
    inserter: Editor item,
    inserterPosition: Maybe Position
  }

type alias Item item =
  {
    priority: Priority,
    editor: Editor item
  }

type Position =
  Before String |
  After String

type Action action =
  None |
  ItemAction {
    url: String,
    action: Editor.Action action
  } |
  Delete String |
  InserterAction (Editor.Action action) |
  SetInserterPosition (Maybe Position)

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
    inserterPosition =
      Nothing
  }

type alias Context item action =
  {
    initialItem: item,
    itemUpdateContext: Editor.UpdateContext item action,
    separator: Separator,
    itemViewContext: Editor.ViewContext item action
  }

type alias Separator =
  {
    html: Html,
    keyCode: KeyCode
  }

update : Context item action -> Address (Editor.Action (Action action)) -> Editor.Action (Action action) -> ListEditor item -> Update (ListEditor item)
update context address action model =
  Editor.update (updateContext context) address action model

updateContext : Context item action -> Editor.UpdateContext (Model item) (Action action)
updateContext context =
  {
    valueChanged _ _ model =
      Component.return model,
    childAdded listAddress snapshot model =
      let result =
            Component.returnAndRun
              { model | items <- model.items |> Dict.insert url item }
              itemUpdate.task
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
    childRemoved listAddress snapshot model =
      let result =
            model.items |> Dict.get url |> Maybe.map (\item ->
              Component.returnAndRun
                { model | items <- model.items |> Dict.remove url }
                (item.editor |> unsubscribe |> TaskUtil.swallowError "ElmFire.unsubscribe failed")
            ) |> Maybe.withDefault (Component.return model)
          url =
            snapshot.reference |> ElmFire.toUrl
          itemAddress =
            listAddress `Signal.forwardTo` (getItemAction url)
      in result,
    childMoved _ snapshot model =
      let result =
            Component.return
              { model | items <-
                model.items |> Dict.update url (Maybe.map (\item ->
                  { item | priority <- snapshot.priority }
                ))
              }
          url =
            snapshot.reference |> ElmFire.toUrl
      in result,
    customAction listAddress action model =
      case action of
        None ->
          Component.return model
        ItemAction itemAction ->
          model.items |> Dict.get itemAction.url |> Maybe.map (\item ->
            let result =
                  Component.returnAndRun
                    { model | items <- updatedItems }
                    itemUpdate.task
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
          ) |> Maybe.withDefault (Component.return model)
        Delete url ->
          Component.returnAndRun
            model
            (
              url
              |> ElmFire.fromUrl
              |> ElmFire.remove
              |> TaskUtil.swallowError "Failed to delete item"
            )
        InserterAction inserterAction ->
          let result =
                Component.returnAndRun
                  { model |
                    inserter <-
                      inserterUpdate.model,
                    inserterPosition <-
                      model.inserterPosition -- TODO Nothing if saved
                  }
                  inserterUpdate.task
              inserterUpdate =
                { inserter | priority <- inserterPriority model }
                |> Editor.update context.itemUpdateContext inserterAddress inserterAction
              inserter =
                model.inserter
              inserterAddress =
                listAddress `Signal.forwardTo` InserterAction
          in result
        SetInserterPosition inserterPosition ->
          Component.return
            { model | inserterPosition <- inserterPosition }
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
  |> Task.Extra.parallel
  |> Task.map (always ())

inserterPriority : Model item -> Priority
inserterPriority model =
  model.inserterPosition |> Maybe.map (\inserterPosition ->
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
          case inserterPosition of
            After urlBefore ->
              indicesByUrl |> Dict.get urlBefore |> Maybe.withDefault Random.minInt
            Before urlAfter ->
              indicesByUrl |> Dict.get urlAfter |> Maybe.map (\otherIndex -> otherIndex - 1) |> Maybe.withDefault Random.minInt
        indexAfter =
          case inserterPosition of
            Before urlAfter ->
              indicesByUrl |> Dict.get urlAfter |> Maybe.withDefault Random.maxInt
            After urlBefore ->
              indicesByUrl |> Dict.get urlBefore |> Maybe.map (\otherIndex -> otherIndex + 1) |> Maybe.withDefault Random.maxInt
    in result
  ) |> Maybe.withDefault (0.0 |> NumberPriority)

view : Context item action -> Address (Editor.Action (Action action)) -> ListEditor item -> Html
view context address editor =
  Editor.view (viewContext context) address editor

viewContext : Context item action -> Editor.ViewContext (Model item) (Action action)
viewContext context =
  {
    view listAddress editor =
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
                maybeInserter (Before url)
                ++ [
                  viewTransformer context.separator False listAddress editor url,
                  Editor.view context.itemViewContext (itemAddress url) item.editor,
                  viewTransformer context.separator True listAddress editor url
                ]
                ++ maybeInserter (After url)
              )
              |> List.intersperse [context.separator.html]
              |> List.concat
          items =
            editor.model |> get
          itemAddress url =
            listAddress `Signal.forwardTo` (getItemAction url)
          maybeInserter inserterPosition =
            if editor.model.inserterPosition == Just inserterPosition then
              [viewInserter context listAddress editor]
            else
              []
      in result
  }

viewInserter : Context item action -> Address (Action action) -> ListEditor item -> Html
viewInserter context listAddress editor =
  Editor.view context.itemViewContext (listAddress `Signal.forwardTo` InserterAction) editor.model.inserter

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
          maybeSeparator
      keyUpAction keyCode =
        if keyCode == removerKey.keyCode then
          Delete url
        else if keyCode == separator.keyCode then
          SetInserterPosition (Just inserterPosition)
        else if keyCode == inserterHidingKey.keyCode then
          SetInserterPosition Nothing
        else
          None
      removerKey =
        if after then backspace else delete
      inserterHidingKey =
        if after then delete else backspace
      inserterPosition =
        if after then After url else Before url
      maybeSeparator =
        if (after && editor.model.inserterPosition == Just (After url)) || (not after && editor.model.inserterPosition == Just (Before url)) then
          [separator.html]
        else []
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
