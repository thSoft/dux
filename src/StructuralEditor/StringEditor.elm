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

-- TODO indeterministic content race condition in Firefox

type alias Model =
  {
    inputText: String
  }

type Action =
  None |
  SetInputText String |
  Save

init : String -> Model
init inputText =
  {
    inputText =
      inputText
  }

update : (Model -> Task Never Action) -> Action -> Model -> Update Model Action
update onSave action model =
  case action of
    None ->
      Component.return model
    SetInputText inputText ->
      Component.return
        { model | inputText <- inputText }
    Save ->
      {
        model =
          model,
        effects =
          onSave model |> Effects.task
      }

view : List Attribute -> String -> Address Action -> Model -> Html
view extraAttributes initialInputText address model =
  let result =
        Html.span
          (attributes ++ extraAttributes)
          [
            initialInputText |> Html.text
          ]
      attributes =
        [
          Attributes.contenteditable True,
          handleKeys False [enter.keyCode],
          Events.on "input" inputTextDecoder handleInput,
          Events.onKeyUp address keyUpAction
        ]
      handleInput inputText =
        inputText |> SetInputText |> Signal.message address
      keyUpAction key =
        if key == enter.keyCode then Save else None
  in result

handleKeys : Bool -> List KeyCode -> Html.Attribute
handleKeys enable keyCodes =
  let result =
        Attributes.attribute "onkeydown"
          ("if ([" ++ keyCodesPrinted ++ "].indexOf(event.keyCode) " ++ operator ++ " -1) {
            event.preventDefault();
          }") -- XXX https://github.com/evancz/elm-html/issues/83
      keyCodesPrinted =
        keyCodes |> List.map toString |> String.join ", "
      operator =
        if enable then "==" else ">"
  in result

inputTextDecoder : Decoder String
inputTextDecoder =
  Decode.at ["target", "textContent"] Decode.string
