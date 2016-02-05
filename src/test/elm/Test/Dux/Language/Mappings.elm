module Test.Dux.Environment.Language.Mappings where

import Task exposing (Task)
import Html exposing (Html)
import Dux.Language.Types exposing (..)
import Dux.Language.Mappings as Mappings
import FirebaseModel.Mapping as Mapping

baseUrl : String
baseUrl = "https://thsoft.firebaseio.com/DUX/test/"

functionType : Mapping.Output FunctionType
functionType =
  Mapping.mirror Mappings.functionType (baseUrl ++ "FunctionType")

numberLiteral : Mapping.Output NumberLiteral
numberLiteral =
  Mapping.mirror Mappings.numberLiteral (baseUrl ++ "NumberLiteral")

functionCall : Mapping.Output FunctionCall
functionCall =
  Mapping.mirror Mappings.functionCall (baseUrl ++ "FunctionCall")

expression : Mapping.Output Expression
expression =
  Mapping.mirror Mappings.expression (baseUrl ++ "Expression")

main : Signal Html
main =
  Signal.map4
    (\a b c d ->
      Html.div
        []
        [
          a |> toText "FunctionType",
          b |> toText "NumberLiteral",
          c |> toText "FunctionCall",
          d |> toText "Expression"
        ]
    )
    functionType.model
    numberLiteral.model
    functionCall.model
    expression.model

toText : String -> a -> Html
toText title a =
  Html.div
    []
    [title ++ ": " ++ (a |> toString) |> Html.text]

port functionTypeTasks : Signal (Task () ())
port functionTypeTasks =
  functionType.tasksToRun

port numberLiteralTasks : Signal (Task () ())
port numberLiteralTasks =
  numberLiteral.tasksToRun

port functionCallTasks : Signal (Task () ())
port functionCallTasks =
  functionCall.tasksToRun

port expressionTasks : Signal (Task () ())
port expressionTasks =
  expression.tasksToRun
