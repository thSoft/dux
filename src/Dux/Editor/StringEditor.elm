module Dux.Editor.StringEditor where

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
import ElmFireSync.Ref as Ref

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

update : Ref.Model String -> Action -> Model -> Update Model Action
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

view : Ref.Model String -> (Ref.Error -> String) -> Address Action -> Model -> Html
view ref showError address model =
  let result =
        Html.div
          [
            Attributes.contenteditable True,
            Attributes.attribute "onkeydown"
              "if ([13].indexOf(event.keyCode) > -1) {
                event.preventDefault();
              }", -- XXX https://github.com/evancz/elm-html/issues/83
            Events.on "input" inputTextDecoder handleInput,
            Events.on "keyup" Events.keyCode handleKeyUp
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
      handleKeyUp key =
        let result =
              action |> Signal.message address
            action =
              if key == enter.keyCode then
                Save
              else
                None
        in result
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
