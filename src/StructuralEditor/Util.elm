module StructuralEditor.Util where

import Html
import Html.Attributes as Attributes

bordered : String -> Html.Attribute
bordered borderColor =
  Attributes.style [
    ("display", "inline"),
    ("border", "1px solid " ++ borderColor),
    ("border-radius", "4px"),
    ("padding", "2px"),
    ("margin", "1px")
  ]
