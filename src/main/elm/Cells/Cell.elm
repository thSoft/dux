module Cells.Cell where

import Signal exposing (Address)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Cells.Types exposing (..)
import Combobox

htmlCell : id -> String -> Html -> Cell id
htmlCell id text html =
  {
    id =
      id,
    content =
      Atomic {
        html =
          html,
        text =
          "",
        menu =
          Nothing
      },
    leftMenu =
      Nothing,
    rightMenu =
      Nothing
  }

textCell : id -> String -> Cell id
textCell id text =
  htmlCell id text (text |> Html.text)

view : Address (Action id) -> Editor id -> Cell id -> Html
view address model cell =
  let result =
        Html.span
          []
          (leftMenuView ++ [contentView] ++ rightMenuView)
      leftMenuView =
        viewMenu address model cell Left cell.leftMenu
      contentView =
        viewCellContent address model cell cell.content
      rightMenuView =
        viewMenu address model cell Right cell.rightMenu
  in result

viewCellContent : Address (Action id) -> Editor id -> Cell id -> CellContent id -> Html
viewCellContent address model cell cellContent =
  let result =
        Html.span
          []
          children
      children =
        case cellContent of
          Atomic atomic ->
            Html.div
              [
                Events.onClick address (Select {
                  selection =
                    Just {
                      cellId =
                        cell.id,
                      menuId =
                        Content
                    },
                  inputText =
                    atomic.text
                })
              ]
              [atomic.html]
            :: (viewMenu address model cell Content atomic.menu)
          Composite composite ->
            composite.children |> List.map (view address model)
  in result

viewMenu : Address (Action id) -> Editor id -> Cell id -> MenuId -> Menu -> List Html
viewMenu address model cell menuId menu =
  Maybe.map2 (\commands selection ->
    if (selection.cellId == cell.id) && (selection.menuId == menuId) then
      [
        Combobox.view
          {
            inputText =
              case cell.content of
                Atomic atomic ->
                  atomic.text
                Composite _ ->
                  "",
            commands =
              commands,
            style =
              Combobox.Input,
            extraAttributes =
              [Attributes.autofocus True]
          }
          (address `Signal.forwardTo` ComboboxAction)
          model.combobox
      ]
    else
      []
  ) menu model.selection
  |> Maybe.withDefault []
