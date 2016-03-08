module Cells.Editor where

import Html exposing (Html)
import Component exposing (Component, Update)
import Combobox
import Cells.Types exposing (..)

component : Html -> Component (Editor id) (Action id)
component html =
  {
    init = \_ ->
      init,
    update = \_ ->
      update,
    view = \_ _ ->
      html,
    inputs =
      []
  }

init : Update (Editor id)
init =
  {
    selection =
      Nothing,
    combobox =
      Combobox.init ""
  }
  |> Component.return

update : Action id -> Editor id -> Update (Editor id)
update action editor =
  case action of
    None ->
      Component.return editor
    Select { selection, inputText } ->
      let result =
            Component.return
              { editor |
                selection =
                  selection,
                combobox =
                  { combobox |
                    inputText =
                      inputText
                  }
              }
          combobox =
            editor.combobox
      in result
    ComboboxAction comboboxAction ->
      let result =
            Component.returnAndRun
              { editor |
                combobox =
                  comboboxUpdate.model
              }
              comboboxUpdate.task
          comboboxUpdate =
            editor.combobox |> Combobox.update comboboxAction
      in result
