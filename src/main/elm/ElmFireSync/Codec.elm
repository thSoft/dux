module ElmFireSync.Codec where

import Json.Decode as Decode exposing (Value, Decoder)
import Json.Encode as Encode

type alias Codec a =
  {
    decoder: Decoder a,
    encode: a -> Value
  }

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
