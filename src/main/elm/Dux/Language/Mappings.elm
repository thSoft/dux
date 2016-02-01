module Dux.Language.Mappings where

import Json.Decode as Decode
import FirebaseModel.Mapping as Mapping exposing (Mapping)
import Dux.Language.Types exposing (..)

functionType : Mapping FunctionType
functionType =
  Decode.customDecoder
    Decode.string
    (\string ->
      case string of
        "add" ->
          Ok Add
        "subtract" ->
          Ok Subtract
        "multiply" ->
          Ok Multiply
        "divide" ->
          Ok Divide
        _ ->
          Err "unknown function type"
    )
  |> Mapping.fromDecoder
{-
numberLiteral : Mapping NumberLiteral
numberLiteral =
  Mapping.object1
    NumberLiteral
    ("value" := Mapping.fromDecoder Decode.float)

functionCall : Mapping FunctionCall
functionCall =
  Mapping.object3
    FunctionCall
    ("functionType" := functionType)
    ("firstArgument" := expression)
    ("secondArgument" := expression)

expression : Mapping Expression
expression =
  Mapping.oneOf
    [
      numberLiteral |> Mapping.map NumberLiteralExpression,
      functionCall |> Mapping.map FunctionCallExpression
    ]
-}
