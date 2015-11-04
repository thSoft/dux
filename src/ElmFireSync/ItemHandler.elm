module ElmFireSync.ItemHandler where

import Task exposing (Task)
import Effects exposing (Never)
import ElmFire
import Component exposing (Update)
import ElmFireSync.Ref exposing (Ref)

type alias ItemHandler model action =
  {
    init: String -> Update model action,
    done: model -> Task Never action,
    update: action -> model -> Update model action
  }
