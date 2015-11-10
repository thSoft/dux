module ElmFireSync.ItemHandler where

import Signal exposing (Address)
import Component exposing (Update, HandledTask)

type alias ItemHandler model action =
  {
    init: Address action -> String -> Update model,
    done: Address action -> model -> HandledTask,
    update: Address action -> action -> model -> Update model
  }
