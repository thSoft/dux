module Test.Dux.Environment.Language.Mappings where

import Task exposing (Task)
import Html exposing (Html)
import Dux.Language.Types exposing (..)
import Dux.Language.Mappings as Mappings
import FirebaseModel.Mapping as Mapping

functionType : Mapping.Output FunctionType
functionType =
  Mapping.mirror Mappings.functionType "https://thsoft.firebaseio.com/DUX/test/FunctionType"

numberLiteral : Mapping.Output NumberLiteral
numberLiteral =
  Mapping.mirror Mappings.numberLiteral "https://thsoft.firebaseio.com/DUX/test/NumberLiteral"

expression : Mapping.Output Expression
expression =
  Mapping.mirror Mappings.expression "https://thsoft.firebaseio.com/DUX/test/Expression"

main : Signal Html
main =
  Signal.map3
    (\a b c ->
      Html.div
        []
        [
          a |> toText,
          b |> toText,
          c |> toText
        ]
    )
    functionType.model
    numberLiteral.model
    expression.model

toText : a -> Html
toText a =
  Html.div
    []
    [a |> toString |> Html.text]

port functionTypeTasks : Signal (Task () ())
port functionTypeTasks =
  functionType.tasksToRun

port numberLiteralTasks : Signal (Task () ())
port numberLiteralTasks =
  numberLiteral.tasksToRun

port expressionTasks : Signal (Task () ())
port expressionTasks =
  expression.tasksToRun
