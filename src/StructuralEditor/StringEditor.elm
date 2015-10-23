module StructuralEditor.StringEditor where

import String
import Signal exposing (Address)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Task
import Effects exposing (Effects)
import Debug
import Keyboard.Keys exposing (..)
import TaskUtil
import Component exposing (Update)
import ElmFireSync.Ref as Ref exposing (Ref)

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

update : Ref String -> Action -> Model -> Update Model Action
update ref action model =
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
          ref |> Ref.set model.inputText
          |> TaskUtil.toEffects None "ElmFire.set failed"
      }

view : Ref String -> (Ref.Error -> String) -> Address Action -> Model -> Html
view ref showError address model =
  let result =
        Html.span
          [
            Attributes.contenteditable True,
            handleKeys False [enter],
            Events.on "input" inputTextDecoder handleInput,
            Events.onKeyUp address keyUpAction,
            Attributes.style [
              ("border", "1px solid gray"),
              ("border-radius", "4px"),
              ("padding", "2px"),
              ("margin", "1px")
            ]
          ]
          [
            string |> Html.text
          ]
      string =
        case ref |> Ref.get of
          Err error ->
            error |> showError
          Ok data ->
            data
      handleInput inputText =
        inputText |> SetInputText |> Signal.message address
      keyUpAction key =
        if key == enter.keyCode then Save else None
  in result

handleKeys : Bool -> List Key -> Html.Attribute
handleKeys enable keys =
  let result =
        Attributes.attribute "onkeydown"
          ("if ([" ++ keyCodes ++ "].indexOf(event.keyCode) " ++ operator ++ " -1) {
            event.preventDefault();
          }") -- XXX https://github.com/evancz/elm-html/issues/83
      keyCodes =
        keys |> List.map (\key -> key.keyCode |> toString) |> String.join ", "
      operator =
        if enable then "==" else ">"
  in result

inputTextDecoder : Decoder String
inputTextDecoder =
  Decode.at ["target", "textContent"] Decode.string

stringHandler : Ref.Handler String
stringHandler =
  {
    decoder =
      Decode.string,
    encode =
      Encode.string
  }
