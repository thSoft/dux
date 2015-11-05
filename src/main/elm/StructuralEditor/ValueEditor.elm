module StructuralEditor.ValueEditor where

import Keyboard exposing (KeyCode)
import Json.Decode as Decode exposing (Decoder)
import String
import Signal exposing (Address)
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events
import Task exposing (Task)
import Effects exposing (Never)
import Keyboard.Keys exposing (..)
import Component exposing (Update)
import TaskUtil
import ElmFireSync.Codec as Codec
import ElmFireSync.Ref as Ref exposing (Ref)
import StructuralEditor.Combobox as Combobox
import StructuralEditor.EditorKind exposing (EditorKind)
import StructuralEditor.Styles as Styles

type alias Model a =
  {
    kind: EditorKind a,
    url: String,
    ref: Ref a,
    combobox: Combobox.Model
  }

type Action a =
  None |
  RefAction (Ref.Action a) |
  ComboboxAction Combobox.Action

init : EditorKind a -> String -> Address (Action a) -> Update (Model a) (Action a)
init kind url address =
  let result =
        {
          model =
            {
              url =
                url,
              kind =
                kind,
              ref =
                initRef.model,
              combobox =
                initCombobox
            },
          effects =
            initRef.effects |> Effects.map RefAction
        }
      initRef =
        Ref.init
          kind.codec
          url
          (address `Signal.forwardTo` RefAction)
      initCombobox =
        Combobox.init ""
  in result

comboboxContext : Model a -> Combobox.Context
comboboxContext model =
  {
    inputText =
      model |> getInputText,
    commands =
      model.combobox.inputText
      |> model.kind.stringConverter.fromString
      |> Maybe.map (\value ->
        if modified model then
          [
            {
              label =
                "Set to " ++ (value |> toString),
              task =
                model.ref
                |> Ref.set value
                |> TaskUtil.swallowError () "ElmFire.set failed"
            }
          ]
        else
          []
      )
      |> Maybe.withDefault [],
    style =
      Combobox.ContentEditable,
    extraAttributes =
      []
  }

getInputText : Model a -> String
getInputText model =
  model.ref
  |> Ref.get
  |> Result.toMaybe
  |> Maybe.map model.kind.stringConverter.toString
  |> Maybe.withDefault ""

modified : Model a -> Bool
modified model =
  (model |> getInputText) /= model.combobox.inputText

update : Action a -> Model a -> Update (Model a) (Action a)
update action model =
  case action of
    None ->
      Component.return model
    RefAction refAction ->
      let result =
            {
              model =
                { modelWithUpdatedRef |
                  combobox <- updatedCombobox },
              effects =
                updateRef.effects |> Effects.map RefAction
            }
          updateRef =
            Ref.update refAction model.ref
          modelWithUpdatedRef =
            { model | ref <- updateRef.model }
          updatedCombobox =
            case refAction of
              Ref.ValueChanged _ -> -- XXX handle editing conflict
                { combobox | inputText <- modelWithUpdatedRef |> getInputText }
              _ ->
                combobox
          combobox =
            model.combobox
      in result
    ComboboxAction comboboxAction ->
      let result =
            {
              model =
                { model | combobox <- updateCombobox.model },
              effects =
                updateCombobox.effects |> Effects.map ComboboxAction
            }
          updateCombobox =
            Combobox.update
              (comboboxContext model)
              comboboxAction
              model.combobox
      in result

view : Address (Action a) -> Model a -> Html
view address model =
  let result =
        Html.div
          [
            Attributes.style <| Styles.bordered borderColor
          ]
          [viewCombobox]
      borderColor =
        if modified model then
          "red"
        else
          "gray"
      viewCombobox =
        Combobox.view
          (comboboxContext model)
          (address `Signal.forwardTo` ComboboxAction)
          model.combobox
  in result
