module Dux.Language.Types where

import FirebaseModel.Mapping exposing (..)

type FunctionType =
  Add |
  Subtract |
  Multiply |
  Divide

type alias NumberLiteral =
  {
    value: Stored Float
  }

type alias FunctionCall =
  {
    functionType: Stored FunctionType,
    firstArgument: Stored Expression,
    secondArgument: Stored Expression
  }

type Expression =
  NumberLiteralExpression NumberLiteral |
  FunctionCallExpression FunctionCall
