module StructuralEditor.ValueEditor where

import Task exposing (Task)
import Signal exposing (Address)
import Json.Decode as Decode exposing (Value, Decoder)
import Html exposing (Html)
import Html.Attributes as Attributes
import TaskUtil
import ElmFire exposing (Reference, Location)
import Component exposing (Component, Update)
import StructuralEditor.Editor as Editor exposing (Editor)
import Combobox
import StructuralEditor.Styles as Styles

type alias ValueEditor data =
  Editor (Model data)

type alias Model data =
  {
    data: Result NoData data,
    combobox: Combobox.Model
  }

type NoData =
  Loading |
  DecodingFailed String

type Action =
  ComboboxAction Combobox.Action

component : Context data -> Location -> Component (ValueEditor data) (Editor.Action Action)
component context location =
  {
    init =
      init location,
    update =
      update context,
    view =
      view context True,
    inputs =
      []
  }

init : Location -> Address (Editor.Action Action) -> Update (ValueEditor data)
init location address =
  Editor.init initialModel location address

initialModel : Model data
initialModel =
  {
    data =
      Err Loading,
    combobox =
      Combobox.init ""
  }

type alias Context data =
  {
    codec: Codec data,
    stringConverter: StringConverter data
  }

type alias Codec a =
  {
    decoder: Decoder a,
    encode: a -> Value
  }

type alias StringConverter a =
  {
    toString: a -> String,
    fromString: String -> List a
  }

update : Context data -> Address (Editor.Action Action) -> Editor.Action Action -> ValueEditor data -> Update (ValueEditor data)
update context address action editor =
  Editor.update (updateContext context) address action editor

updateContext : Context data -> Editor.UpdateContext (ValueEditor data) Action
updateContext context =
  {
    valueChanged = \_ snapshot editor ->
      let result =
            Component.return
              { editor | model = updatedModel }
          updatedModel =
              { model |
                data =
                  data,
                combobox =
                  Combobox.update comboboxAction model.combobox |> .model
              }
          model =
            editor.model
          data =
            snapshot.value
            |> Decode.decodeValue context.codec.decoder
            |> Result.formatError DecodingFailed
          comboboxAction =
            data
            |> Result.toMaybe
            |> Maybe.map (\value ->
              value |> context.stringConverter.toString |> Combobox.SetInputText
            )
            |> Maybe.withDefault Combobox.None
      in result,
    childAdded = \_ _ editor ->
      Component.return editor,
    childRemoved = \_ _ editor ->
      Component.return editor,
    childMoved = \_ _ editor ->
      Component.return editor,
    customAction = \_ action editor ->
      case action of
        ComboboxAction comboboxAction ->
          let result =
                Component.returnAndRun
                  { editor | model = updatedModel }
                  updateCombobox.task
              updateCombobox =
                Combobox.update
                  comboboxAction
                  model.combobox
              updatedModel =
                { model | combobox = updateCombobox.model }
              model =
                editor.model
          in result
  }

view : Context data -> Bool -> Address (Editor.Action Action) -> ValueEditor data -> Html
view context focused address editor =
  Editor.view (viewContext context) focused address editor

viewContext : Context data -> Editor.ViewContext (Model data) Action
viewContext context =
  {
    view = \focused address editor ->
      let result =
            Html.div
              [Attributes.style <| Styles.bordered borderColor]
              [viewCombobox]
          borderColor =
            if editor.model |> modified context.stringConverter then
              "red"
            else
              "gray"
          viewCombobox = -- TODO distinguish error state with image
            Combobox.view
              (comboboxContext focused context editor)
              (address `Signal.forwardTo` ComboboxAction)
              editor.model.combobox
      in result
  }

comboboxContext : Bool -> Context data -> ValueEditor data -> Combobox.Context
comboboxContext focused context editor =
  {
    inputText =
      editor.model |> getInputText context.stringConverter,
    commands =
      editor.model.combobox.inputText
      |> context.stringConverter.fromString
      |> List.concatMap (\value ->
        if (editor.model.data /= Ok value) || (editor.model |> modified context.stringConverter) then
          [
            {
              label =
                "Set to " ++ (value |> toString),
              task =
                editor
                |> set context.codec value
                |> TaskUtil.swallowError "ElmFire.set failed"
            }
          ]
        else
          []
      ),
    style =
      Combobox.ContentEditable,
    extraAttributes =
      focusAttributes focused
  }

focusAttributes : Bool -> List Html.Attribute
focusAttributes focused =
  if focused then
    [Attributes.attribute "data-autofocus" "true"]
  else
    []

getInputText : StringConverter data -> Model data -> String
getInputText stringConverter model =
  model.data
  |> Result.toMaybe
  |> Maybe.map stringConverter.toString
  |> Maybe.withDefault ""

modified : StringConverter data -> Model data -> Bool
modified stringConverter model =
  (model |> getInputText stringConverter) /= model.combobox.inputText

-- Misc

set : Codec data -> data -> ValueEditor data -> Task ElmFire.Error Reference
set codec value editor =
  let result =
        ElmFire.setWithPriority json priority location -- ElmFire.set would clear priority
      json =
        value |> codec.encode
      priority =
        editor.priority
      location =
        editor.location
  in result
