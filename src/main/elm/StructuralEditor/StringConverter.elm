module StructuralEditor.StringConverter where

type alias StringConverter a =
  {
    toString: a -> String,
    fromString: String -> Maybe a
  }

string : StringConverter String
string =
  {
    toString =
      identity,
    fromString =
      Just
  }
