module StructuralEditor.ListEditor where

import Array
import Random
import Dict exposing (Dict)
import Signal exposing (Address, Mailbox)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import ElmFire exposing (Location, Reference, Priority(..))
import Keyboard.Keys exposing (..)
import Component exposing (Update)
import TaskUtil
import ElmFireSync.ItemHandler exposing (ItemHandler)
import ElmFireSync.Ref as Ref exposing (Ref)
import ElmFireSync.ValueRef as ValueRef exposing (ValueRef)
import ElmFireSync.ListRef as ListRef exposing (ListRef)
import StructuralEditor.Combobox as Combobox
import StructuralEditor.EditorKind exposing (EditorKind)
import StructuralEditor.ValueEditor as ValueEditor
import StructuralEditor.Separator as Separator exposing (Separator)
import StructuralEditor.Styles as Styles

-- TODO fix lost focus when deleting last item or before cursor
-- TODO move to next/previous element when pressing enter or right/left
-- TODO implement move
-- TODO implement priority fixing
-- TODO fix child added & moved displayed as separate steps when inserting

type alias Model model =
  {
    itemKind: EditorKind model,
    ref: ListRef (ValueEditor.Model model),
    inserter: Combobox.Model,
    inserterPosition: Maybe Position
  }

type Position =
  Before String |
  After String

type Action action =
  None |
  Delete String |
  RefAction (Ref.Action (ListRef.Action (ValueEditor.Action action))) |
  InserterAction Combobox.Action |
  SetInserterPosition (Maybe Position)

init : EditorKind model -> Location -> Address (Action action) -> Update (Model model)
init itemKind location address =
  let result =
        Component.returnAndRun
          {
            itemKind =
              itemKind,
            ref =
              initRef.model,
            inserter =
              Combobox.init "",
            inserterPosition =
              Nothing
          }
          initRef.task
      initRef =
        ListRef.init
          location
          (address `Signal.forwardTo` RefAction)
  in result

update : Address (Action action) -> Action action -> Model model -> Update (Model model)
update address action model =
  case action of
    None ->
      Component.return model
    Delete url ->
      Component.returnAndRun
        model
        (
          url
          |> ElmFire.fromUrl
          |> ElmFire.remove
          |> TaskUtil.swallowError "Failed to delete item"
        )
    SetInserterPosition inserterPosition ->
      let result =
            Component.return
              { model |
                inserterPosition <-
                  inserterPosition,
                inserter <-
                  { oldInserter | inputText <- "" }
              }
          oldInserter =
            model.inserter
      in result
    RefAction refAction ->
      let result =
            Component.returnAndRun
              { model | ref <- refUpdate.model }
              refUpdate.task
          refUpdate =
            ListRef.update
              (editorItemHandler model.itemKind)
              (address `Signal.forwardTo` RefAction)
              refAction
              model.ref
      in result
    InserterAction inserterAction ->
      let result =
            Component.returnAndRun
              { model |
                inserter <-
                  inserterUpdate.model,
                inserterPosition <-
                  case inserterAction of
                    Combobox.Submit _ ->
                      Nothing
                    _ ->
                      model.inserterPosition
              }
              inserterUpdate.task
          inserterUpdate =
            model.inserter |> Combobox.update (inserterContext model) inserterAction
      in result

editorItemHandler : EditorKind model -> ItemHandler (ValueEditor.Model model) (ValueEditor.Action action)
editorItemHandler itemKind =
  {
    init address url =
      ValueEditor.init
        itemKind
        (url |> ElmFire.fromUrl)
        address,
    done _ model =
      model.ref
      |> Ref.unsubscribe
      |> TaskUtil.swallowError "Unsubscription failed",
    update =
      ValueEditor.update
  }

view : Separator -> Address (Action action) -> Model model -> Html
view separator address model =
  let result =
        Html.div
          [
            Attributes.style [
              ("padding", "0.36em")
            ]
          ]
          itemViews
      itemViews =
        if items |> List.isEmpty then
          [viewInserter address model]
        else
          items
          |> List.map (\(url, item) ->
            maybeInserter (Before url)
            ++ [
              viewTransformer separator False address model url item,
              viewEditor address model url item,
              viewTransformer separator True address model url item
            ]
            ++ maybeInserter (After url)
          )
          |> List.intersperse [separator.html]
          |> List.concat
      items =
        model.ref |> ListRef.get
      maybeInserter inserterPosition =
        if model.inserterPosition == Just inserterPosition then
          [viewInserter address model]
        else
          []
  in result

viewEditor : Address (Action action) -> Model model -> String -> ListRef.Item (ValueEditor.Model model) -> Html
viewEditor address model url item =
  ValueEditor.view
    (address `Signal.forwardTo` (\itemAction ->
      {
        url =
          url,
        action =
          itemAction
      } |> Ref.CustomAction |> RefAction)
    )
    item.data

viewTransformer : Separator -> Bool -> Address (Action action) -> Model model -> String -> ListRef.Item (ValueEditor.Model model) -> Html
viewTransformer separator after address model url item =
  let result =
        Html.span
          [
            Attributes.contenteditable True,
            Attributes.style [
              ("padding", transformerSize)
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
        if (after && model.inserterPosition == Just (After url)) || (not after && model.inserterPosition == Just (Before url)) then
          [separator.html]
        else []
  in result

transformerSize : String
transformerSize =
  "0.1em"

viewInserter : Address (Action action) -> Model model -> Html
viewInserter address model =
  Html.div -- wrapping instead of specifying extraAttributes because of https://github.com/Matt-Esch/virtual-dom/issues/176
    [
      Attributes.style <|
        ("margin-left", transformerSize) ::
        Styles.bordered "lightgray"
    ]
    [
      model.inserter
      |> Combobox.view
        (inserterContext model)
        (address `Signal.forwardTo` InserterAction)
    ]

inserterContext : Model model -> Combobox.Context
inserterContext model =
  let result =
        {
          inputText =
            "",
          commands =
            commands,
          style =
            Combobox.ContentEditable,
          extraAttributes =
            [Attributes.attribute "data-autofocus" ""]
        }
      commands =
        model.inserter.inputText
        |> model.itemKind.stringConverter.fromString
        |> List.map (\value ->
          {
            label =
              "Insert " ++ (value |> toString),
            task =
              model.ref.location
              |> ElmFire.push
              |> ElmFire.open
              |> TaskUtil.andThen (\reference ->
                reference |> inserterRef model |> ValueRef.set model.itemKind.codec value
              )
              |> TaskUtil.andThen (\reference ->
                reference |> ElmFire.location |> ElmFire.setPriority (inserterPriority model)
              )
              |> TaskUtil.swallowError "Failed to insert item"
          }
        )
  in result

inserterRef : Model model -> Reference -> ValueRef model
inserterRef model reference =
  Ref.init
    ValueRef.initialModel
    (reference |> ElmFire.location)
    inserterRefActionMailbox.address -- dummy
  |> .model

inserterRefActionMailbox : Mailbox (Ref.Action action)
inserterRefActionMailbox =
  Signal.mailbox Ref.None

inserterPriority : Model model -> Priority
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
          model.ref |> ListRef.get
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
