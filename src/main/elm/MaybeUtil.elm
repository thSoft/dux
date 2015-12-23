module MaybeUtil where

isJust : Maybe a -> Bool
isJust maybe =
  case maybe of
    Just _ ->
      True
    _ ->
      False
