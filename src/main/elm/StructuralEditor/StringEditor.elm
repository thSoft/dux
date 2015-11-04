module StructuralEditor.StringEditor where

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
import StructuralEditor.Util as Util

type alias Model =
  {
    ref: Ref String,
    combobox: Combobox.Model
  }

type Action =
  None |
  RefAction (Ref.Action String) |
  ComboboxAction Combobox.Action

init : String -> Address Action -> Update Model Action
init url address =
  let result =
        {
          model =
            {
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
          {
            url =
              url,
            codec =
              Codec.string
          }
          (address `Signal.forwardTo` RefAction)
      initCombobox =
        Combobox.init ""
  in result

comboboxContext : Model -> Combobox.Context
comboboxContext model =
  {
    inputText =
      model.ref |> getInputText,
    commands =
      if modified model then
        [
          {
            label =
              "Set to “" ++ model.combobox.inputText ++ "”",
            task =
              model.ref
              |> Ref.set model.combobox.inputText
              |> TaskUtil.swallowError () "ElmFire.set failed"
          }
        ]
      else
        [],
    style =
      Combobox.ContentEditable,
    extraAttributes =
      []
  }

getInputText : Ref String -> String
getInputText ref =
  ref |> Ref.get |> Result.toMaybe |> Maybe.withDefault ""

modified : Model -> Bool
modified model =
  (model.ref |> getInputText) /= model.combobox.inputText

update : Action -> Model -> Update Model Action
update action model =
  case action of
    None ->
      Component.return model
    RefAction refAction ->
      let result =
            {
              model =
                { model |
                  ref <- updateRef.model,
                  combobox <- updatedCombobox },
              effects =
                updateRef.effects |> Effects.map RefAction
            }
          updateRef =
            Ref.update refAction model.ref
          updatedCombobox =
            case refAction of
              Ref.ValueChanged _ -> -- XXX handle editing conflict
                { combobox | inputText <- updateRef.model |> getInputText }
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

view : Address Action -> Model -> Html
view address model =
  let result =
        Html.div
          [
            Util.bordered borderColor
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
