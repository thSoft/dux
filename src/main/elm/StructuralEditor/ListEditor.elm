module StructuralEditor.ListEditor where

import Debug
import Array
import Random
import Keyboard exposing (KeyCode)
import String
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Signal exposing (Address, Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Effects exposing (Effects, Never)
import ElmFire exposing (Location, Reference, Priority(..))
import Keyboard.Keys exposing (..)
import Component exposing (Update)
import TaskUtil
import ElmFireSync.Codec as Codec
import ElmFireSync.ItemHandler exposing (ItemHandler)
import ElmFireSync.Ref as Ref exposing (Ref)
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

type alias Model a =
  {
    itemKind: EditorKind a,
    context: Context,
    listRef: ListRef (ValueEditor.Model a) (ValueEditor.Action a),
    inserter: Combobox.Model,
    inserterPosition: Maybe Position
  }

type alias Context =
  {
    url: String,
    separator: Separator
  }

type Position =
  Before String |
  After String

type Action a =
  None |
  Delete (ListRef.Item (ValueEditor.Model a)) |
  ListRefAction (ListRef.Action (ValueEditor.Action a)) |
  InserterAction Combobox.Action |
  SetInserterPosition (Maybe Position)

init : EditorKind a -> Context -> Address (Action a) -> Update (Model a) (Action a)
init itemKind context address =
  let result =
        {
          model =
            {
              itemKind =
                itemKind,
              context =
                context,
              listRef =
                initListRef.model,
              inserter =
                Combobox.init "",
              inserterPosition =
                Nothing
            },
          effects =
            initListRef.effects |> Effects.map ListRefAction
        }
      initListRef =
        ListRef.init (editorItemHandler listRefAddress itemKind) context.url listRefAddress
      listRefAddress =
        address `Signal.forwardTo` ListRefAction
  in result

editorItemHandler : Address (ListRef.Action (ValueEditor.Action a)) -> EditorKind a -> ItemHandler (ValueEditor.Model a) (ValueEditor.Action a)
editorItemHandler address itemKind =
  {
    init url =
      ValueEditor.init itemKind url (address `Signal.forwardTo` (ListRef.ItemAction url)),
    done model =
      model.ref
      |> Ref.unsubscribe
      |> TaskUtil.swallowError ValueEditor.None "Unsubscription failed",
    update =
      ValueEditor.update
  }

update : Address (Action a) -> Action a -> Model a -> Update (Model a) (Action a)
update address action model =
  case action of
    None ->
      Component.return model
    Delete item ->
      {
        model =
          model,
        effects =
          item.data.ref
          |> Ref.delete
          |> TaskUtil.swallowError None "Failed to delete item"
          |> Effects.task
      }
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
    ListRefAction listRefAction ->
      let result =
            {
              model =
                { model | listRef <- listRefUpdate.model },
              effects =
                listRefUpdate.effects |> Effects.map ListRefAction
            }
          listRefUpdate =
            ListRef.update (address `Signal.forwardTo` ListRefAction) listRefAction model.listRef
      in result
    InserterAction inserterAction ->
      let result =
            {
              model =
                { model |
                  inserter <-
                    inserterUpdate.model,
                  inserterPosition <-
                    case inserterAction of
                      Combobox.Submit _ ->
                        Nothing
                      _ ->
                        model.inserterPosition
                },
              effects =
                inserterUpdate.effects |> Effects.map InserterAction
            }
          inserterUpdate =
            model.inserter |> Combobox.update (inserterContext model) inserterAction
      in result

view : Address (Action a) -> Model a -> Html
view address model =
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
              viewTransformer False address model url item,
              viewEditor address model url item,
              viewTransformer True address model url item
            ]
            ++ maybeInserter (After url)
          )
          |> List.intersperse [model.context.separator.html]
          |> List.concat
      items =
        model.listRef |> ListRef.get
      maybeInserter inserterPosition =
        if model.inserterPosition == Just inserterPosition then
          [viewInserter address model]
        else
          []
  in result

viewEditor : Address (Action a) -> Model a -> String -> ListRef.Item (ValueEditor.Model a) -> Html
viewEditor address model url item =
  ValueEditor.view
    (address `Signal.forwardTo` (\itemAction ->
      itemAction |> ListRef.ItemAction url |> ListRefAction)
    )
    item.data

viewTransformer : Bool -> Address (Action a) -> Model a -> String -> ListRef.Item (ValueEditor.Model a) -> Html
viewTransformer after address model url item =
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
          Delete item
        else if keyCode == model.context.separator.keyCode then
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
          [model.context.separator.html]
        else []
  in result

transformerSize : String
transformerSize =
  "0.1em"

viewInserter : Address (Action a) -> Model a -> Html
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

inserterContext : Model a -> Combobox.Context
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
        |> Maybe.map (\value ->
          [
            {
              label =
                "Insert " ++ (value |> toString),
              task =
                model.listRef.url
                |> ElmFire.fromUrl
                |> ElmFire.push
                |> ElmFire.open
                |> TaskUtil.andThen (\reference ->
                  reference |> inserterRef model |> Ref.set value
                )
                |> TaskUtil.andThen (\reference ->
                  reference |> ElmFire.location |> ElmFire.setPriority (inserterPriority model)
                )
                |> TaskUtil.swallowError () "Failed to insert item"
            }
          ]
        )
        |> Maybe.withDefault []
  in result

inserterRef : Model a -> Reference -> Ref a
inserterRef model reference =
  Ref.init
    model.itemKind.codec
    (reference |> ElmFire.toUrl)
    inserterRefActionMailbox.address -- dummy
  |> .model

inserterRefActionMailbox : Mailbox (Ref.Action a)
inserterRefActionMailbox =
  Signal.mailbox Ref.None

inserterPriority : Model a -> Priority
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
          model.listRef |> ListRef.get
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
