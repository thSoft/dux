module Dux.Environment.Mappings where

import FirebaseModel.Mapping as Mapping exposing (Mapping)
import Dux.Environment.Types exposing (..)
import Dux.Language.Mappings exposing (..)

workspace : Mapping Workspace
workspace =
  Mapping.object Workspace
    `Mapping.withField` {
      key = "views",
      get = .views,
      mapping = Mapping.many expressionView
    }

expressionView : Mapping ExpressionView
expressionView =
  Mapping.object ExpressionView
    `Mapping.withField` {
      key = "expression",
      get = .expression,
      mapping = Mapping.reference expression
    }
