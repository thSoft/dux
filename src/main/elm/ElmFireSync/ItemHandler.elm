module ElmFireSync.ItemHandler where

import Task exposing (Task)
import Effects exposing (Never)
import Component exposing (Update)

type alias ItemHandler model action =
  {
    init: String -> Update model action,
    done: model -> Task Never action,
    update: action -> model -> Update model action
  }
