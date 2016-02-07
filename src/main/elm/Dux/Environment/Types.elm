module Dux.Environment.Types where

import FirebaseModel.Mapping exposing (Reference, Stored)
import Dux.Language.Types exposing (..)

type alias Workspace =
  {
    views: List ExpressionView
  }

type alias ExpressionView =
  {
    expression: Stored (Reference Expression)
  }
