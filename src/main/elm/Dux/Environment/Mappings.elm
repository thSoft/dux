module Dux.Environment.Mappings where

import FirebaseModel.Mapping as Mapping exposing (Mapping, (:=))
import Dux.Environment.Types exposing (..)
import Dux.Language.Mappings exposing (..)

workspace : Mapping Workspace
workspace =
  Mapping.object1
    Workspace
    {
      key = "views",
      get = .views,
      mapping = Mapping.many expressionView
    }

expressionView : Mapping ExpressionView
expressionView =
  Mapping.object1
    ExpressionView
    {
      key = "expression",
      get = .expression,
      mapping = Mapping.reference expression
    }
