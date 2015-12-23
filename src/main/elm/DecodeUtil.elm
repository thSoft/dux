module DecodeUtil where

import Json.Decode as Decode
import MaybeUtil

canBeDecodedWith : Decode.Decoder a -> Decode.Value -> Bool
canBeDecodedWith decoder value =
  value
  |> Decode.decodeValue decoder
  |> Result.toMaybe
  |> MaybeUtil.isJust
