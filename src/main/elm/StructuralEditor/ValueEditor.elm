module StructuralEditor.ValueEditor where

import Signal exposing (Address)
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Component exposing (Update)
import TaskUtil
import ElmFire exposing (Location)
import ElmFireSync.Ref as Ref exposing (Ref)
import StructuralEditor.Combobox as Combobox
import StructuralEditor.EditorKind exposing (EditorKind)
import StructuralEditor.Styles as Styles

type alias Model a =
  {
    kind: EditorKind a,
    ref: Ref a,
    combobox: Combobox.Model
  }

type Action a =
  None |
  RefAction (Ref.Action a) |
  ComboboxAction Combobox.Action

init : EditorKind a -> Location -> Address (Action a) -> Update (Model a)
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
        Ref.init
          kind.codec
          location
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
      |> List.concatMap (\value ->
        if ((model.ref |> Ref.get) /= Ok value) || (modified model) then
          [
            {
              label =
                "Set to " ++ (value |> toString),
              task =
                model.ref
                |> Ref.set value
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

update : Action a -> Model a -> Update (Model a)
update action model =
  case action of
    None ->
      Component.return model
    RefAction refAction ->
      let result =
            Component.returnAndRun
              { modelWithUpdatedRef | combobox <- updatedCombobox }
              updateRef.task
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
            Component.returnAndRun
              { model | combobox <- updateCombobox.model }
              updateCombobox.task
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
      viewCombobox = -- TODO distinguish error state with image
        Combobox.view
          (comboboxContext model)
          (address `Signal.forwardTo` ComboboxAction)
          model.combobox
  in result
