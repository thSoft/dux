module StructuralEditor.Combobox where

import Keyboard exposing (KeyCode)
import Array
import String
import Regex
import Time
import Signal exposing (Address, Message)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Task exposing (Task)
import Effects exposing (Effects, Never)
import Keyboard.Keys exposing (..)
import Component exposing (Update)

-- TODO cancel

-- Model

type alias Model =
  {
    inputText: String,
    commandIndex: Int,
    menuVisible: Bool
  }

type alias Context =
  {
    initialInputText: String,
    commands: List Command,
    style: Style,
    extraAttributes: List Html.Attribute
  }

type alias Command =
  {
    label: String,
    task: Task Never ()
  }

type Style =
  Input |
  ContentEditable

init : String -> Model
init inputText =
  {
    inputText =
      inputText,
    commandIndex =
      0,
    menuVisible =
      False
  }

-- Update

type Action =
  None |
  SetInputText String |
  SetMenuVisible Bool |
  Submit Command |
  MoveToNext |
  MoveToPrevious |
  MoveToFirst |
  MoveToLast

update : Context -> Action -> Model -> Update Model Action
update context action model =
  case action of
    None ->
      Component.return model
    SetInputText inputText ->
      Component.return
        { model | inputText <- inputText }
    SetMenuVisible menuVisible ->
      Component.return
        { model | menuVisible <- menuVisible }
    Submit command ->
      {
        model =
          model,
        effects =
          command.task
          |> Task.map (always None)
          |> Effects.task
      }
    MoveToNext ->
      Component.return (moveBy 1 context model)
    MoveToPrevious ->
      Component.return (moveBy -1 context model)
    MoveToFirst ->
      Component.return
        { model |
          commandIndex <-
            0
        }
    MoveToLast ->
      Component.return
        { model |
          commandIndex <-
            (getVisibleCommands context model |> List.length) - 1
        }

moveBy : Int -> Context -> Model -> Model
moveBy delta context model =
  let result =
        if visibleItems |> List.isEmpty then
          model
        else
          { model |
            commandIndex <-
              (model.commandIndex + delta) % (visibleItems |> List.length)
          }
      visibleItems =
        getVisibleCommands context model
  in result

-- View

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
              if | key == enter.keyCode -> submit
                 | key == arrowDown.keyCode -> MoveToNext
                 | key == arrowUp.keyCode -> MoveToPrevious
                 | key == pageDown.keyCode -> MoveToLast
                 | key == pageUp.keyCode -> MoveToFirst
                 | otherwise -> None
        in action |> Signal.message address
      submit =
        getVisibleCommands context model
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
              [Attributes.value context.initialInputText]
            ContentEditable ->
              [Attributes.property "textContent" (context.initialInputText |> Encode.string)]
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
                  ("box-shadow", "0px 2px 2px 2px #aaaaaa")
                ]
              ]
              commands
          ]
      commands =
        visibleCommands
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
      visibleCommands =
        getVisibleCommands context model
      selectedStyle selected =
        if selected then
          [
            ("background", "linear-gradient(to bottom, #0066ee, #0033cc)"),
            ("color", "white")
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

getVisibleCommands : Context -> Model -> List Command
getVisibleCommands context model =
  if model.inputText == "" then
    context.commands
  else
    context.commands |> List.filter (\command ->
      command.label |> fuzzyContains model.inputText
    )

fuzzyContains : String -> String -> Bool
fuzzyContains needle haystack =
  needle |> String.words |> List.all (\word ->
    if word |> String.isEmpty then
      False
    else
      haystack |> containsIgnoreCase word
  )

containsIgnoreCase : String -> String -> Bool
containsIgnoreCase needle haystack =
  haystack |> Regex.contains (needle |> Regex.escape |> Regex.regex |> Regex.caseInsensitive)

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