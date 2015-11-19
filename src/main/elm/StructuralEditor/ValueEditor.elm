module StructuralEditor.ValueEditor where

import Signal exposing (Address)
import Html exposing (Html)
import Html.Attributes as Attributes
import Component exposing (Update)
import TaskUtil
import ElmFire exposing (Location)
import ElmFireSync.Ref as Ref
import ElmFireSync.ValueRef as ValueRef exposing (ValueRef)
import StructuralEditor.Combobox as Combobox
import StructuralEditor.EditorKind exposing (EditorKind)
import StructuralEditor.Styles as Styles

type alias Model model =
  {
    kind: EditorKind model,
    ref: ValueRef model,
    combobox: Combobox.Model
  }

type Action action =
  None |
  RefAction (Ref.Action action) |
  ComboboxAction Combobox.Action

init : EditorKind model -> Location -> Address (Action action) -> Update (Model model)
init kind location address =
  let result =
        Component.returnAndRun
          {
            kind =
              kind,
            ref =
              initRef.model,
            combobox =
              initCombobox
          }
          initRef.task
      initRef =
        ValueRef.init
          location
          (address `Signal.forwardTo` RefAction)
      initCombobox =
        Combobox.init ""
  in result

comboboxContext : Model model -> Combobox.Context
comboboxContext model =
  {
    inputText =
      model |> getInputText,
    commands =
      model.combobox.inputText
      |> model.kind.stringConverter.fromString
      |> List.concatMap (\value ->
        if ((model.ref |> Ref.getModel) /= Ok value) || (modified model) then
          [
            {
              label =
                "Set to " ++ (value |> toString),
              task =
                model.ref
                |> ValueRef.set model.kind.codec value
                |> TaskUtil.swallowError "ElmFire.set failed"
            }
          ]
        else
          []
      ),
    style =
      Combobox.ContentEditable,
    extraAttributes =
      []
  }

getInputText : Model model -> String
getInputText model =
  model.ref
  |> Ref.getModel
  |> Result.toMaybe
  |> Maybe.map model.kind.stringConverter.toString
  |> Maybe.withDefault ""

modified : Model model -> Bool
modified model =
  (model |> getInputText) /= model.combobox.inputText

update : Address (Action action) -> Action action -> Model model -> Update (Model model)
update address action model =
  case action of
    None ->
      Component.return model
    RefAction refAction ->
      let result =
            Component.returnAndRun
              { modelWithUpdatedRef | combobox <- updatedCombobox }
              updateRef.task
          updateRef =
            ValueRef.update
              model.kind.codec
              (address `Signal.forwardTo` RefAction)
              refAction
              model.ref
          modelWithUpdatedRef =
            { model | ref <- updateRef.model }
          updatedCombobox =
            case refAction of
              Ref.Event eventType _ ->
                if eventType == Ref.valueChanged then
                  { combobox | inputText <- modelWithUpdatedRef |> getInputText } -- XXX handle editing conflict
                else
                  combobox
              _ ->
                combobox
          combobox =
            model.combobox
      in result
    ComboboxAction comboboxAction ->
      let result =
            Component.returnAndRun
              { model | combobox <- updateCombobox.model }
              updateCombobox.task
          updateCombobox =
            Combobox.update
              (comboboxContext model)
              comboboxAction
              model.combobox
      in result

view : Address (Action action) -> Model model -> Html
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
      viewCombobox = -- TODO distinguish error state with image
        Combobox.view
          (comboboxContext model)
          (address `Signal.forwardTo` ComboboxAction)
          model.combobox
  in result
