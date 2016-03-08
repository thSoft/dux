module Cells.Types where

import Html exposing (Html)
import Combobox

type alias Editor id =
  {
    selection: Selection id,
    combobox: Combobox.Model
  }

type alias Cell id =
  {
    id: id,
    content: CellContent id,
    leftMenu: Menu,
    rightMenu: Menu
  }

type CellContent id =
  Atomic {
    html: Html,
    text: String,
    menu: Menu
  } |
  Composite {
    children: List (Cell id)
  }

type alias Menu =
  Maybe (List Combobox.Command)

type alias Selection id =
  Maybe {
    cellId: id,
    menuId: MenuId
  }

type MenuId =
  Left |
  Right |
  Content

type Action id =
  None |
  Select {
    selection: Selection id,
    inputText: String
  } |
  ComboboxAction Combobox.Action
