module StructuralEditor.Combobox where

import Keyboard exposing (KeyCode)
import Array
import String
import Signal exposing (Address, Message)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Task exposing (Task)
import Keyboard.Keys exposing (..)
import Component exposing (Update)

-- TODO fix click on menu commands due to onBlur
-- TODO highlight input text in commands
-- TODO command description

type alias Model =
  {
    inputText: String,
    commandIndex: Int,
    menuVisible: Bool
  }

type alias Context =
  {
    inputText: String,
    commands: List Command,
    style: Style,
    extraAttributes: List Html.Attribute
  }

type alias Command =
  {
    label: String,
    task: Task () ()
  }

type Style =
  Input |
  ContentEditable

init : String -> Model
init initialInputText =
  {
    inputText =
      initialInputText,
    commandIndex =
      0,
    menuVisible =
      False
  }

type Action =
  None |
  SetInputText String |
  SetCommandIndex Int |
  SetMenuVisible Bool |
  Submit Command

update : Context -> Action -> Model -> Update Model
update context action model =
  case action of
    None ->
      Component.return model
    SetInputText inputText ->
      Component.return
        { model |
          inputText <- inputText,
          menuVisible <- True
        }
    SetCommandIndex commandIndex ->
      Component.return (
        if model.menuVisible then
          { model | commandIndex <- commandIndex }
        else
          { model | menuVisible <- True }
      )
    SetMenuVisible menuVisible ->
      Component.return
        { model | menuVisible <- menuVisible }
    Submit command ->
      Component.returnAndRun
        { model | menuVisible <- False }
        command.task

view : Context -> Address Action -> Model -> Html
view context address model =
  let result =
        Html.span
          []
          (input :: menu)
      input =
        case context.style of
          Input ->
            Html.input
              attributes
              []
          ContentEditable ->
            Html.span
              (Attributes.contenteditable True :: attributes)
              []
      attributes =
        (eventHandlers ++ styleAttributes ++ textContent ++ context.extraAttributes)
      eventHandlers =
        [
          Events.on "input" (inputTextDecoder context.style) handleInput,
          handleKeys False [
            enter.keyCode,
            arrowDown.keyCode,
            arrowUp.keyCode
          ],
          Events.on "keyup" Events.keyCode handleKeyUp,
          Events.onFocus address (SetMenuVisible True),
          Events.onBlur address (SetMenuVisible False)
        ]
      handleInput inputText =
        SetInputText inputText |> Signal.message address
      handleKeyUp key =
        let action =
              if key == enter.keyCode then
                submit
              else if key == escape.keyCode then
                SetMenuVisible False
              else if key == arrowDown.keyCode then
                SetCommandIndex next
              else if key == arrowUp.keyCode then
                SetCommandIndex previous
              else if key == pageDown.keyCode then
                SetCommandIndex last
              else if key == pageUp.keyCode then
                SetCommandIndex first
              else
                None
            first =
              0
            last =
              (context.commands |> List.length) - 1
            next =
              moveBy 1
            previous =
              moveBy -1
            moveBy delta =
              if context.commands |> List.isEmpty then
                model.commandIndex
              else
                (model.commandIndex + delta) % (context.commands |> List.length)
        in action |> Signal.message address
      submit =
        context.commands
        |> Array.fromList
        |> Array.get model.commandIndex
        |> Maybe.map Submit
        |> Maybe.withDefault None
      styleAttributes =
        [
          Attributes.style [
            ("position", "relative")
          ]
        ]
      textContent =
          case context.style of
            Input ->
              [Attributes.value context.inputText]
            ContentEditable ->
              [Attributes.property "textContent" (context.inputText |> Encode.string)]
      menu =
        if (not model.menuVisible) || (commands |> List.isEmpty) then
          []
        else
          [
            Html.div
              [
                Attributes.style [
                  ("position", "absolute"),
                  ("z-index", "1"),
                  ("display", "table"),
                  ("box-shadow", "0 2px 8px 1px rgba(0, 0, 0, 0.3)"),
                  ("border-radius", "4px")
                ]
              ]
              commands
          ]
      commands =
        context.commands
        |> List.indexedMap (\index command ->
          Html.div
            [
              Attributes.style (
                ("padding", "2px") ::
                (selectedStyle (index == model.commandIndex))
              ),
              Events.onClick address (Submit command)
            ]
            [
              Html.text command.label
            ]
        )
      selectedStyle selected =
        if selected then
          [
            ("background", "linear-gradient(to bottom, #eeeeee, #dddddd)")
          ]
        else
          []
 in result

inputTextDecoder : Style -> Decoder String
inputTextDecoder style =
  case style of
    Input ->
      Events.targetValue
    ContentEditable ->
      Decode.at ["target", "textContent"] Decode.string

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
