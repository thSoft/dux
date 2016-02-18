module Dux.Language.Mappings where

import String
import Json.Decode as Decode
import Json.Encode as Encode
import FirebaseModel.Mapping as Mapping exposing (Mapping)
import Dux.Language.Types exposing (..)

functionType : Mapping FunctionType
functionType =
  Mapping.fromCodec
    (\model ->
      model |> toString |> String.toLower |> Encode.string
    )
    (Decode.customDecoder
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
    )

numberLiteral : Mapping NumberLiteral
numberLiteral =
  Mapping.object NumberLiteral
    `Mapping.withField` {
      key = "value",
      get = .value,
      mapping = Mapping.fromCodec Encode.float Decode.float
    }

functionCall : Mapping FunctionCall
functionCall =
  Mapping.recursive <| \() ->
    Mapping.object FunctionCall
      `Mapping.withField` {
        key = "functionType",
        get = .functionType,
        mapping = functionType
      }
      `Mapping.withField` {
        key = "firstArgument",
        get = .firstArgument,
        mapping = Mapping.recursive <| \() -> expression
      }
      `Mapping.withField` {
        key = "secondArgument",
        get = .secondArgument,
        mapping = Mapping.recursive <| \() -> expression
      }

expression : Mapping Expression
expression =
  Mapping.recursive <| \() ->
    Mapping.choice
      `Mapping.withOption` {
        typeName = "numberLiteral",
        constructor = NumberLiteralExpression,
        selector = \expression ->
          case expression of
            NumberLiteralExpression numberLiteralExpression ->
              Just numberLiteralExpression
            _ ->
              Nothing,
        mapping = numberLiteral
      }
      `Mapping.withOption` {
        typeName = "functionCall",
        constructor = FunctionCallExpression,
        selector = \expression ->
          case expression of
            FunctionCallExpression functionCallExpression ->
              Just functionCallExpression
            _ ->
              Nothing,
        mapping = Mapping.recursive <| \() -> functionCall
      }
