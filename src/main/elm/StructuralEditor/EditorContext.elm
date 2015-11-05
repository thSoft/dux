module StructuralEditor.EditorContext where

import ElmFireSync.Codec as Codec
import ElmFireSync.Ref as Ref

type alias EditorContext a =
  {
    ref: Ref.Context a,
    toString: a -> String,
    fromString: String -> Maybe a
  }

string : String -> EditorContext String
string url =
  {
    ref =
      {
        url =
          url,
        codec =
          Codec.string
      },
    toString =
      identity,
    fromString =
      Just
  }
