module Dux.Environment.Mappings where

import FirebaseModel.Mapping as Mapping exposing (Mapping, (:=))
import Dux.Environment.Types exposing (..)
import Dux.Language.Mappings exposing (..)
{-
workspace : Mapping Workspace
workspace =
  Mapping.object1
    Workspace
    ("views" := Mapping.list expressionView)
-}
expressionView : Mapping ExpressionView
expressionView =
  Mapping.object1
    ExpressionView
    ("expression" := Mapping.reference expression)
