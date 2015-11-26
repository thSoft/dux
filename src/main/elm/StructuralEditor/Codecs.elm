module StructuralEditor.Codecs where

import Json.Decode as Decode exposing (Value, Decoder)
import Json.Encode as Encode
import StructuralEditor.ValueEditor exposing (Codec)

string : Codec String
string =
  {
    decoder =
      Decode.string,
    encode =
      Encode.string
  }

int : Codec Int
int =
  {
    decoder =
      Decode.int,
    encode =
      Encode.int
  }

float : Codec Float
float =
  {
    decoder =
      Decode.float,
    encode =
      Encode.float
  }

bool : Codec Bool
bool =
  {
    decoder =
      Decode.bool,
    encode =
      Encode.bool
  }
