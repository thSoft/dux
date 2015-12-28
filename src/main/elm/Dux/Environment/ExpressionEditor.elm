module Dux.Environment.ExpressionEditor where

import Signal exposing (Address)
import Html exposing (Html)
import Json.Decode as Decode exposing ((:=))
import ElmFire exposing (Location)
import DecodeUtil
import TaskUtil
import Component exposing (Component, Update)
import StructuralEditor.Editor as Editor exposing (Editor)
import Dux.Environment.NumberLiteralEditor as NumberLiteralEditor
import Dux.Environment.FunctionTypeEditor as FunctionTypeEditor

-- Expression

type alias ExpressionEditor =
  Editor Model

type Model =
  NumberLiteral (Editor NumberLiteralEditor.Model) |
  FunctionCall (Editor FunctionCallEditorModel)

type Action =
  NumberLiteralAction (Editor.Action NumberLiteralEditor.Action) |
  FunctionCallAction (Editor.Action FunctionCallEditorAction)

component : Location -> Component ExpressionEditor (Editor.Action Action)
component location =
  {
    init =
      init location,
    update =
      update,
    view =
      view True,
    inputs =
      []
  }

init : Location -> Address (Editor.Action Action) -> Update (Editor Model)
init location address =
  let result =
        { initEditor | task = task }
      task =
        [
          initNumberLiteral.task,
          initEditor.task
        ] |> TaskUtil.parallel
      initEditor =
        Editor.init (initNumberLiteral.model |> NumberLiteral) location address
      initNumberLiteral =
        NumberLiteralEditor.init location (address `Signal.forwardTo` (NumberLiteralAction >> Editor.CustomAction))
  in result

update : Address (Editor.Action Action) -> Editor.Action Action -> Editor Model -> Update (Editor Model)
update address action editor =
  Editor.update updateContext address action editor

updateContext : Editor.UpdateContext ExpressionEditor Action
updateContext =
  let result =
        { defaultUpdateContext |
          customAction = customAction,
          valueChanged = valueChanged
        }
      defaultUpdateContext =
        Editor.defaultUpdateContext
      customAction address action editor =
        case action of
          NumberLiteralAction numberLiteralAction ->
            case editor.model of
              NumberLiteral numberLiteral ->
                let result =
                      Component.returnAndRun
                        { editor | model = updatedModel }
                        task
                    updatedModel =
                      updateNumberLiteral.model |> NumberLiteral
                    task =
                      updateNumberLiteral.task
                    updateNumberLiteral =
                      NumberLiteralEditor.update (address `Signal.forwardTo` NumberLiteralAction) numberLiteralAction numberLiteral
                in result
              _ ->
                Component.return editor
          FunctionCallAction functionCallAction ->
            case editor.model of
              FunctionCall functionCall ->
                let result =
                      Component.returnAndRun
                        { editor | model = updatedModel }
                        task
                    updatedModel =
                      updateFunctionCall.model |> FunctionCall
                    task =
                      updateFunctionCall.task
                    updateFunctionCall =
                      functionCallEditorUpdate (address `Signal.forwardTo` FunctionCallAction) functionCallAction functionCall
                in result
              _ ->
                Component.return editor
      valueChanged address snapshot editor =
        if snapshot.value |> NumberLiteralEditor.isValid then
          case editor.model of
            NumberLiteral numberLiteralEditor ->
              Component.return editor
            FunctionCall functionCallEditor ->
              let result =
                    Component.returnAndRun
                      { editor | model = initNumberLiteral.model |> NumberLiteral }
                      task
                  task =
                    [
                      initNumberLiteral.task,
                      editor |> Editor.done
                    ] |> TaskUtil.parallel
                  initNumberLiteral =
                    NumberLiteralEditor.init editor.location (address `Signal.forwardTo` NumberLiteralAction)
              in result
        else if snapshot.value |> functionCallEditorIsValid then
          case editor.model of
            FunctionCall functionCallEditor ->
              Component.return editor
            NumberLiteral numberLiteralEditor ->
              let result =
                    Component.returnAndRun
                      { editor | model = initFunctionCall.model |> FunctionCall }
                      task
                  task =
                    [
                      initFunctionCall.task,
                      editor |> Editor.done
                    ] |> TaskUtil.parallel
                  initFunctionCall =
                    functionCallEditorInit editor.location (address `Signal.forwardTo` FunctionCallAction)
              in result
        else
          Component.return editor
  in result

view : Bool -> Address (Editor.Action Action) -> Editor Model -> Html
view focused address editor =
  Editor.view viewContext focused address editor

viewContext : Editor.ViewContext Model Action
viewContext =
  {
    view = \focused address editor ->
      case editor.model of
        NumberLiteral numberLiteral ->
          NumberLiteralEditor.view focused (address `Signal.forwardTo` NumberLiteralAction) numberLiteral
        FunctionCall functionCall ->
          functionCallEditorView focused (address `Signal.forwardTo` FunctionCallAction) functionCall
  }

decoder : Decode.Decoder Bool
decoder =
  Decode.oneOf [
    functionCallDecoder |> Decode.map (always True),
    NumberLiteralEditor.context |> .codec |> .decoder |> Decode.map (always True)
  ]

-- Function call
-- XXX included here to avoid circular dependency

type alias FunctionCallEditor =
  Editor FunctionCallEditorModel

type alias FunctionCallEditorModel =
  {
    functionType: Editor FunctionTypeEditor.Model,
    firstArgument: Editor Model,
    secondArgument: Editor Model
  }

type FunctionCallEditorAction =
  FunctionTypeAction (Editor.Action FunctionTypeEditor.Action) |
  FirstArgumentAction (Editor.Action Action) |
  SecondArgumentAction (Editor.Action Action)

functionCallEditorComponent : Location -> Component FunctionCallEditor (Editor.Action FunctionCallEditorAction)
functionCallEditorComponent location =
  {
    init =
      functionCallEditorInit location,
    update =
      functionCallEditorUpdate,
    view =
      functionCallEditorView True,
    inputs =
      []
  }

functionCallEditorInit : Location -> Address (Editor.Action FunctionCallEditorAction) -> Update (Editor FunctionCallEditorModel)
functionCallEditorInit location address =
  let result =
        { initEditor | task =
          [
            initFunctionType.task,
            initFirstArgument.task,
            initSecondArgument.task
          ]
          |> TaskUtil.parallel
        }
      initEditor =
        Editor.init initialModel location address
      initialModel =
        {
          functionType =
            initFunctionType.model,
          firstArgument =
            initFirstArgument.model,
          secondArgument =
            initSecondArgument.model
        }
      initFunctionType =
        FunctionTypeEditor.init (location |> ElmFire.sub functionTypeField) (address `Signal.forwardTo` (FunctionTypeAction >> Editor.CustomAction))
      initFirstArgument =
        init (location |> ElmFire.sub firstArgumentField) (address `Signal.forwardTo` (FirstArgumentAction >> Editor.CustomAction))
      initSecondArgument =
        init (location |> ElmFire.sub secondArgumentField) (address `Signal.forwardTo` (SecondArgumentAction >> Editor.CustomAction))
  in result

functionTypeField : String
functionTypeField =
  "functionType"

firstArgumentField : String
firstArgumentField =
  "firstArgument"

secondArgumentField : String
secondArgumentField =
  "secondArgument"

functionCallEditorUpdate : Address (Editor.Action FunctionCallEditorAction) -> Editor.Action FunctionCallEditorAction -> Editor FunctionCallEditorModel -> Update (Editor FunctionCallEditorModel)
functionCallEditorUpdate address action editor =
  Editor.update functionCallEditorUpdateContext address action editor

functionCallEditorUpdateContext : Editor.UpdateContext FunctionCallEditor FunctionCallEditorAction
functionCallEditorUpdateContext =
  let result =
        { defaultUpdateContext | customAction = customAction }
      defaultUpdateContext =
        Editor.defaultUpdateContext
      customAction address action editor =
        case action of
          FunctionTypeAction functionTypeAction ->
            let result =
                  Component.returnAndRun
                    { editor | model = updatedModel }
                    functionTypeUpdate.task
                updatedModel =
                  { model | functionType = functionTypeUpdate.model }
                model =
                  editor.model
                functionTypeUpdate =
                  FunctionTypeEditor.update (address `Signal.forwardTo` FunctionTypeAction) functionTypeAction model.functionType
            in result
          FirstArgumentAction firstArgumentAction ->
            let result =
                  Component.returnAndRun
                    { editor | model = updatedModel }
                    firstArgumentUpdate.task
                updatedModel =
                  { model | firstArgument = firstArgumentUpdate.model }
                model =
                  editor.model
                firstArgumentUpdate =
                  update (address `Signal.forwardTo` FirstArgumentAction) firstArgumentAction model.firstArgument
            in result
          SecondArgumentAction secondArgumentAction ->
            let result =
                  Component.returnAndRun
                    { editor | model = updatedModel }
                    secondArgumentUpdate.task
                updatedModel =
                  { model | secondArgument = secondArgumentUpdate.model }
                model =
                  editor.model
                secondArgumentUpdate =
                  update (address `Signal.forwardTo` SecondArgumentAction) secondArgumentAction model.secondArgument
            in result
  in result

functionCallEditorView : Bool -> Address (Editor.Action FunctionCallEditorAction) -> Editor FunctionCallEditorModel -> Html
functionCallEditorView focused address editor =
  let result =
        Html.div [] [
          firstArgumentView,
          functionTypeView,
          secondArgumentView
        ]
      firstArgumentView =
        view False (address `Signal.forwardTo` (FirstArgumentAction >> Editor.CustomAction)) editor.model.firstArgument
      functionTypeView =
        FunctionTypeEditor.view False (address `Signal.forwardTo` (FunctionTypeAction >> Editor.CustomAction)) editor.model.functionType
      secondArgumentView =
        view False (address `Signal.forwardTo` (SecondArgumentAction >> Editor.CustomAction)) editor.model.secondArgument
  in result

functionCallEditorIsValid : Decode.Value -> Bool
functionCallEditorIsValid value =
  value |> DecodeUtil.canBeDecodedWith functionCallDecoder

functionCallDecoder : Decode.Decoder (Bool, FunctionTypeEditor.Data, Bool)
functionCallDecoder =
  Decode.object3 (,,)
    (firstArgumentField := decoder)
    (functionTypeField := (FunctionTypeEditor.context |> .codec |> .decoder))
    (secondArgumentField := decoder)
