module StructuralEditor.Separators where

import Html exposing (Html)
import Html.Attributes as Attributes
import StructuralEditor.ListEditor exposing (Separator)

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
