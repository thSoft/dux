module Dux.Environment.Types where

import FirebaseModel.Mapping exposing (Stored, Reference, Many)
import Dux.Language.Types exposing (..)

type alias Workspace =
  {
    views: Stored (Many ExpressionView)
  }

type alias ExpressionView =
  {
    expression: Stored (Reference Expression)
  }
