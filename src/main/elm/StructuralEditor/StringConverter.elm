module StructuralEditor.StringConverter where

import String

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

int : StringConverter Int
int =
  {
    toString =
      toString,
    fromString =
      String.toInt >> Result.toMaybe
  }

float : StringConverter Float
float =
  {
    toString =
      toString,
    fromString =
      String.toFloat >> Result.toMaybe
  }

bool : StringConverter Bool
bool =
  {
    toString =
      toString,
    fromString string =
      if "true" |> startsWithIgnoringCase string then
        Just True
      else if "false" |> startsWithIgnoringCase string then
        Just False
      else
        Nothing
  }

startsWithIgnoringCase : String -> String -> Bool
startsWithIgnoringCase pattern string =
  string |> String.startsWith (pattern |> String.toLower)
