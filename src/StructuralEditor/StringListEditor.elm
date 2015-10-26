module StructuralEditor.StringListEditor where

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
import ElmFireSync.Handler as Handler
import ElmFireSync.Ref as Ref exposing (Ref)
import ElmFireSync.RefList as RefList exposing (RefList)
import StructuralEditor.StringEditor as StringEditor

-- TODO implement priority fixing
-- TODO implement move
-- TODO fix focus behavior

type alias Model =
  {
    refList: RefList String,
    editors: Dict String StringEditor.Model,
    adder: StringEditor.Model,
    adderPosition: Maybe Position
  }

type Position =
  Before String |
  After String

type Action =
  None |
  Delete (RefList.Item String) |
  SetAdderPosition (Maybe Position) |
  RefListAction (RefList.Action String) |
  EditorAction EditorId StringEditor.Action

type EditorId =
  Existing String |
  Adder

init : Address Action -> String -> Update Model Action
init address url =
  let result =
        {
          model =
            {
              refList =
                refList.model,
              editors =
                Dict.empty,
              adder =
                StringEditor.init "",
              adderPosition =
                Nothing
            },
          effects =
            refList.effects |> Effects.map RefListAction
        }
      refList =
        RefList.init (address |> forwardToRefList) Handler.stringHandler url
  in result

update : Address Action -> Action -> Model -> Update Model Action
update address action model =
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
      Component.return
        { model | adderPosition <- adderPosition }
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
                      |> StringEditor.init
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
                    editor |> StringEditor.update onSave editorAction
                  onSave editorModel =
                      item.ref |> Ref.set (Just item.priority) editorModel.inputText
                    |> TaskUtil.swallowError StringEditor.None "ElmFire.setWithPriority failed"
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
                        if editorAction == StringEditor.Save then Nothing else model.adderPosition
                    },
                  effects =
                    adderUpdate.effects |> Effects.map (EditorAction Adder)
                }
              adderUpdate =
                model.adder |> StringEditor.update onSave editorAction
              onSave editorModel =
                model.refList.url
                |> ElmFire.fromUrl
                |> ElmFire.push
                |> ElmFire.open
                |> TaskUtil.andThen (\reference ->
                  reference |> adderRef |> Ref.set (Just <| adderPriority model) editorModel.inputText
                )
                |> TaskUtil.swallowError StringEditor.None "ElmFire.setWithPriority failed"
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

view : Separator -> Address Action -> Model -> Html
view separator address model =
  let result =
        Html.div
          []
          itemViews
      itemViews =
        if items |> List.isEmpty then
          [viewAdder address model]
        else
          items |> List.map (\item ->
            maybeAdder (Before item.ref.url)
            ++ [
              viewTransformer False separator address model item,
              viewEditor address model item,
              viewTransformer True separator address model item
            ]
            ++ maybeAdder (After item.ref.url)
          ) |> List.intersperse [separator.html] |> List.concat
      items =
        model.refList |> RefList.get
      maybeAdder adderPosition =
        if model.adderPosition == Just adderPosition then [viewAdder address model] else []
  in result

viewEditor : Address Action -> Model -> RefList.Item String -> Html
viewEditor address model item =
  let result =
        model.editors |> Dict.get url |> Maybe.map (\editor ->
          editor
          |> StringEditor.view
            initialInputText
            (address |> forwardToEditor (Existing url))
          |> wrapEditorHtml (Just item.ref) editor
        ) |> Maybe.withDefault ("Programming error, no editor for " ++ url |> Html.text)
      initialInputText =
        case item.ref |> Ref.get of
          Err error ->
            error |> toString
          Ok string ->
            string
      url =
        item.ref.url
  in result

wrapEditorHtml : Maybe (Ref String) -> StringEditor.Model -> Html -> Html
wrapEditorHtml maybeRef editor editorHtml =
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
          if (ref |> Ref.get) /= Ok editor.inputText then "red"
          else "gray"
        ) |> Maybe.withDefault "lightgray"
  in result

viewTransformer : Bool -> Separator -> Address Action -> Model -> RefList.Item String -> Html
viewTransformer after separator address model item =
  let result =
        Html.span
          [
            Attributes.contenteditable True,
            StringEditor.handleKeys True [removerKey.keyCode, tab.keyCode],
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

viewAdder : Address Action -> Model -> Html
viewAdder address model =
  model.adder
  |> StringEditor.view
    ""
    (address |> forwardToEditor Adder)
  |> wrapEditorHtml Nothing model.adder

adderRef : Reference -> Ref String
adderRef reference =
  Ref.init
    adderRefActionMailbox.address -- dummy
    Handler.stringHandler
    (reference |> ElmFire.toUrl)
  |> .model

adderRefActionMailbox : Mailbox (Ref.Action String)
adderRefActionMailbox =
  Signal.mailbox Ref.None

adderPriority : Model -> Priority
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

forwardToRefList : Address Action -> Address (RefList.Action String)
forwardToRefList address =
  Signal.forwardTo address RefListAction

forwardToEditor : EditorId -> Address Action -> Address StringEditor.Action
forwardToEditor id address =
  Signal.forwardTo address (EditorAction id)
