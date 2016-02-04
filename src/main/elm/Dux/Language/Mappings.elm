module Dux.Language.Mappings where

import Dict
import Json.Decode as Decode
import FirebaseModel.Mapping as Mapping exposing (Mapping, (:=))
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
        unknown ->
          Err <| "Unknown function type: " ++ unknown
    )
  |> Mapping.fromDecoder

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
    (Dict.fromList [
      ("numberLiteral", numberLiteral |> Mapping.map NumberLiteralExpression),
      ("functionCall", functionCall |> Mapping.map FunctionCallExpression)
    ])