module StructuralEditor.EditorKind where

import ElmFireSync.Codec as Codec exposing (Codec)
import ElmFireSync.Ref as Ref
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
