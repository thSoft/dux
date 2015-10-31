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
import ElmFireSync.Handler as Handler exposing (Handler)
import ElmFireSync.Ref as Ref exposing (Ref)
import ElmFireSync.RefList as RefList exposing (RefList)
import StructuralEditor.Combobox as Combobox

-- TODO fix lost focus when deleting last item or before cursor
-- TODO move to next/previous element when pressing enter or right/left
-- TODO implement move
-- TODO implement priority fixing

type alias Model a =
  {
    refList: RefList a,
    editors: Dict String Combobox.Model,
    adder: Combobox.Model,
    adderPosition: Maybe Position
  }

type Position =
  Before String |
  After String

type Action a =
  None |
  Delete (RefList.Item a) |
  SetAdderPosition (Maybe Position) |
  RefListAction (RefList.Action a) |
  EditorAction EditorId Combobox.Action

type EditorId =
  Existing String |
  Adder

init : Context a -> Address (Action a) -> String -> Update (Model a) (Action a)
init context address url =
  let result =
        {
          model =
            {
              refList =
                refList.model,
              editors =
                Dict.empty,
              adder =
                Combobox.init "",
              adderPosition =
                Nothing
            },
          effects =
            refList.effects |> Effects.map RefListAction
        }
      refList =
        RefList.init (address |> forwardToRefList) context.itemHandler url
  in result

type alias Context a =
  {
    toString: a -> String,
    fromString: String -> Maybe a,
    itemHandler: Handler a
  }

update : Context a -> Address (Action a) -> Action a -> Model a -> Update (Model a) (Action a)
update context address action model =
  case action of
    None ->
      Component.return model
    Delete item ->
      {
        model =
          model,
        effects =
          item.ref |> Ref.delete
          |> TaskUtil.swallowError None "ElmFire.remove failed"
          |> Effects.task
      }
    SetAdderPosition adderPosition ->
      let result =
            Component.return
              { model |
                adderPosition <-
                  adderPosition,
                adder <-
                  { oldAdder | inputText <- "" }
              }
          oldAdder =
            model.adder
      in result
    RefListAction refListAction ->
      let updateResult =
            {
              model =
                { model |
                  refList <- refListUpdate.model,
                  editors <- updatedEditors },
              effects =
                refListUpdate.effects |> Effects.map RefListAction
            }
          refListUpdate =
            RefList.update (address |> forwardToRefList) refListAction model.refList
          updatedEditors =
            case refListAction of
              RefList.ChildAdded snapshot ->
                let result =
                      model.editors |> Dict.insert (snapshot.reference |> ElmFire.toUrl) editor
                    editor =
                      snapshot.value
                      |> Decode.decodeValue (Handler.stringHandler |> .decoder)
                      |> Result.toMaybe
                      |> Maybe.withDefault ""
                      |> Combobox.init
                in result
              RefList.ChildRemoved snapshot ->
                model.editors |> Dict.remove (snapshot.reference |> ElmFire.toUrl)
              _ ->
                model.editors
      in updateResult
    EditorAction id editorAction ->
      case id of
        Existing url ->
          (model.editors |> Dict.get url) `Maybe.andThen` (\editor ->
            model.refList.items |> Dict.get url |> Maybe.map (\item ->
              let result =
                    {
                      model =
                        { model | editors <- updatedEditors },
                      effects =
                        editorUpdate.effects |> Effects.map (EditorAction (Existing url))
                    }
                  updatedEditors =
                    model.editors |> Dict.insert url editorUpdate.model
                  editorUpdate =
                    editor |> Combobox.update (editorContext context model item editor) editorAction
              in result
            )
          ) |> Maybe.withDefault (Component.return model)
        Adder ->
          let result =
                {
                  model =
                    { model |
                      adder <-
                        adderUpdate.model,
                      adderPosition <-
                        case editorAction of
                          Combobox.Submit _ ->
                            Nothing
                          _ ->
                            model.adderPosition
                    },
                  effects =
                    adderUpdate.effects |> Effects.map (EditorAction Adder)
                }
              adderUpdate =
                model.adder |> Combobox.update (adderContext context model) editorAction
          in result

type alias Separator =
  {
    html: Html,
    keyCode: KeyCode
  }

commaSeparator : Separator
commaSeparator =
  {
    html =
      "," |> Html.text,
    keyCode =
      188
  }

lineSeparator : Separator
lineSeparator =
  {
    html =
      Html.br
        [
          Attributes.style [
            ("line-height", "1.4")
          ]
        ]
        [],
    keyCode =
      13
  }

view : Context a -> Separator -> Address (Action a) -> Model a -> Html
view context separator address model =
  let result =
        Html.div
          []
          itemViews
      itemViews =
        if items |> List.isEmpty then
          [viewAdder context address model]
        else
          items |> List.map (\item ->
            maybeAdder (Before item.ref.url)
            ++ [
              viewTransformer False separator address model item,
              viewEditor context address model item,
              viewTransformer True separator address model item
            ]
            ++ maybeAdder (After item.ref.url)
          ) |> List.intersperse [separator.html] |> List.concat
      items =
        model.refList |> RefList.get
      maybeAdder adderPosition =
        if model.adderPosition == Just adderPosition then [viewAdder context address model] else []
  in result

viewEditor : Context a -> Address (Action a) -> Model a -> RefList.Item a -> Html
viewEditor context address model item =
  let result =
        model.editors |> Dict.get url |> Maybe.map (\editor ->
          editor
          |> Combobox.view
            (editorContext context model item editor)
            (address |> forwardToEditor (Existing url))
          |> wrapEditorHtml context (Just item.ref) editor
        ) |> Maybe.withDefault ("Programming error, no editor for " ++ url |> Html.text)
      url =
        item.ref.url
  in result

editorContext : Context a -> Model a -> RefList.Item a -> Combobox.Model -> Combobox.Context
editorContext context model item editor =
  let result =
        {
          initialInputText =
            initialInputText,
          commands =
            if modified context item.ref editor then set else [],
          style =
            Combobox.ContentEditable,
          extraAttributes =
            []
        }
      set =
        editor.inputText |> context.fromString |> Maybe.map (\value ->
          [
            {
              label =
                "Set value to " ++ editor.inputText,
              task =
                item.ref |> Ref.set (Just item.priority) value
                |> TaskUtil.swallowError () "ElmFire.setWithPriority failed"
            }
          ]
        ) |> Maybe.withDefault []
      initialInputText =
        case item.ref |> Ref.get of
          Err error ->
            error |> toString -- TODO make error presentation more user-friendly
          Ok value ->
            value |> context.toString
  in result

wrapEditorHtml : Context a -> Maybe (Ref a) -> Combobox.Model -> Html -> Html
wrapEditorHtml context maybeRef editor editorHtml =
  let result =
        Html.div
          [
            Attributes.style [
              ("display", "inline"),
              ("border", "1px solid " ++ borderColor),
              ("border-radius", "4px"),
              ("padding", "2px"),
              ("margin", "1px")
            ]
          ]
          [editorHtml]
      borderColor =
        maybeRef |> Maybe.map (\ref ->
          if modified context ref editor then "red"
          else "gray"
        ) |> Maybe.withDefault "lightgray"

  in result

modified : Context a -> Ref a -> Combobox.Model -> Bool
modified context ref editor =
  (ref |> Ref.get |> Result.map context.toString) /= Ok editor.inputText

viewTransformer : Bool -> Separator -> Address (Action a) -> Model a -> RefList.Item a -> Html
viewTransformer after separator address model item =
  let result =
        Html.span
          [
            Attributes.contenteditable True,
            Combobox.handleKeys True [removerKey.keyCode, tab.keyCode],
            Events.onKeyUp address keyUpAction
          ]
          maybeSeparator
      keyUpAction keyCode =
        if keyCode == removerKey.keyCode then
          Delete item
        else if keyCode == separator.keyCode then
          SetAdderPosition (Just adderPosition)
        else if keyCode == adderHidingKey.keyCode then
          SetAdderPosition Nothing
        else
          None
      removerKey =
        if after then backspace else delete
      adderHidingKey =
        if after then delete else backspace
      adderPosition =
        if after then After url else Before url
      maybeSeparator =
        if (after && model.adderPosition == Just (After url)) || (not after && model.adderPosition == Just (Before url)) then
          [separator.html]
        else []
      url =
        item.ref.url
  in result

viewAdder : Context a -> Address (Action a) -> Model a -> Html
viewAdder context address model =
  model.adder
  |> Combobox.view
    (adderContext context model)
    (address |> forwardToEditor Adder)
  |> wrapEditorHtml context Nothing model.adder

adderContext : Context a -> Model a -> Combobox.Context
adderContext context model =
  let result =
        {
          initialInputText =
            "",
          commands =
            add,
          style =
            Combobox.ContentEditable,
          extraAttributes =
            [Attributes.attribute "data-autofocus" ""]
        }
      add =
        model.adder.inputText |> context.fromString |> Maybe.map (\value ->
          [
            {
              label =
                "Add " ++ model.adder.inputText,
              task =
                model.refList.url
                |> ElmFire.fromUrl
                |> ElmFire.push
                |> ElmFire.open
                |> TaskUtil.andThen (\reference ->
                  reference |> adderRef context |> Ref.set (Just <| adderPriority model) value
                )
                |> TaskUtil.swallowError () "ElmFire.setWithPriority failed"
            }
          ]
        ) |> Maybe.withDefault []
  in result

adderRef : Context a -> Reference -> Ref a
adderRef context reference =
  Ref.init
    adderRefActionMailbox.address -- dummy
    context.itemHandler
    (reference |> ElmFire.toUrl)
  |> .model

adderRefActionMailbox : Mailbox (Ref.Action a)
adderRefActionMailbox =
  Signal.mailbox Ref.None

adderPriority : Model a -> Priority
adderPriority model =
  model.adderPosition |> Maybe.map (\adderPosition ->
    let result =
          ((getPriorityAt indexBefore) + (getPriorityAt indexAfter)) / 2 |> NumberPriority
        getPriorityAt index =
          items |> Array.fromList |> Array.get index |> Maybe.map (\item ->
            case item.priority of
              NumberPriority priority ->
                priority
              _ ->
                0.0
          ) |> Maybe.withDefault ((if index < 0 then Random.minInt else Random.maxInt) |> toFloat)
        indicesByUrl =
          items |> List.indexedMap (\index item ->
            (item.ref.url, index)
          ) |> Dict.fromList
        items =
          model.refList |> RefList.get
        indexBefore =
          case adderPosition of
            After urlBefore ->
              indicesByUrl |> Dict.get urlBefore |> Maybe.withDefault Random.minInt
            Before urlAfter ->
              indicesByUrl |> Dict.get urlAfter |> Maybe.map (\otherIndex -> otherIndex - 1) |> Maybe.withDefault Random.minInt
        indexAfter =
          case adderPosition of
            Before urlAfter ->
              indicesByUrl |> Dict.get urlAfter |> Maybe.withDefault Random.maxInt
            After urlBefore ->
              indicesByUrl |> Dict.get urlBefore |> Maybe.map (\otherIndex -> otherIndex + 1) |> Maybe.withDefault Random.maxInt
    in result
  ) |> Maybe.withDefault (0.0 |> NumberPriority)

forwardToRefList : Address (Action a) -> Address (RefList.Action a)
forwardToRefList address =
  Signal.forwardTo address RefListAction

forwardToEditor : EditorId -> Address (Action a) -> Address Combobox.Action
forwardToEditor id address =
  Signal.forwardTo address (EditorAction id)
