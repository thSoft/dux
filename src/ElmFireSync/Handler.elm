module ElmFireSync.Handler where

import Json.Decode as Decode exposing (Value, Decoder)
import Json.Encode as Encode

type alias Handler a =
  {
    decoder: Decoder a,
    encode: a -> Value
  }

stringHandler : Handler String
stringHandler =
  {
    decoder =
      Decode.string,
    encode =
      Encode.string
  }
