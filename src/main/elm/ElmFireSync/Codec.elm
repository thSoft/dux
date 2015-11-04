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
