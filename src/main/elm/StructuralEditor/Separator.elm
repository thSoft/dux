module StructuralEditor.Separator where

import Keyboard exposing (KeyCode)
import Html exposing (Html)
import Html.Attributes as Attributes

type alias Separator =
  {
    html: Html,
    keyCode: KeyCode
  }

comma : Separator
comma =
  {
    html =
      "," |> Html.text,
    keyCode =
      188
  }

line : Separator
line =
  {
    html =
      Html.br
        [
          Attributes.style [
            ("content", " "),
            ("display", "block"),
            ("margin", "0.25em"),
            ("line-height", "2")
          ]
        ]
        [],
    keyCode =
      13
  }
