module StructuralEditor.EditorKind where

import ElmFireSync.Codec as Codec exposing (Codec)
import StructuralEditor.StringConverter as StringConverter exposing (StringConverter)

type alias EditorKind a =
  {
    codec: Codec a,
    stringConverter: StringConverter a
  }

string : EditorKind String
string =
  {
    codec =
      Codec.string,
    stringConverter =
      StringConverter.string
  }

int : EditorKind Int
int =
  {
    codec =
      Codec.int,
    stringConverter =
      StringConverter.int
  }

float : EditorKind Float
float =
  {
    codec =
      Codec.float,
    stringConverter =
      StringConverter.float
  }

bool : EditorKind Bool
bool =
  {
    codec =
      Codec.bool,
    stringConverter =
      StringConverter.bool
  }
