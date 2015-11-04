module StructuralEditor.Styles where

import Html
import Html.Attributes as Attributes

bordered : String -> List (String, String)
bordered borderColor =
  [
    ("display", "inline"),
    ("border", "1px solid " ++ borderColor),
    ("border-radius", "4px"),
    ("padding", "0.2em")
  ]
