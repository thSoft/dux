module StructuralEditor.StringConverter where

import String

type alias StringConverter a =
  {
    toString: a -> String,
    fromString: String -> List a
  }

string : StringConverter String
string =
  {
    toString =
      identity,
    fromString string =
      [string]
  }

int : StringConverter Int
int =
  {
    toString =
      toString,
    fromString string =
      string |> String.toInt |> toList
  }

toList : Result x a -> List a
toList result =
  result |> Result.toMaybe |> Maybe.map (\value -> [value]) |> Maybe.withDefault []

float : StringConverter Float
float =
  {
    toString =
      toString,
    fromString string =
      string |> String.toFloat |> toList
  }

bool : StringConverter Bool
bool =
  {
    toString =
      toString,
    fromString string =
      if string |> String.isEmpty then
        []
      else if "true" |> startsWithIgnoringCase string then
        [True]
      else if "false" |> startsWithIgnoringCase string then
        [False]
      else
        []
  }

startsWithIgnoringCase : String -> String -> Bool
startsWithIgnoringCase pattern string =
  string |> String.toLower |> String.startsWith (pattern |> String.toLower)
