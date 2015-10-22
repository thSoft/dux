module TaskUtil where

import Task exposing (Task)
import Effects exposing (Effects)
import Debug

andThen : (a -> Task x b) -> Task x a -> Task x b
andThen = flip Task.andThen

onError : (x -> Task y a) -> Task x a -> Task y a
onError = flip Task.onError

toEffects : action -> String -> Task x a -> Effects action
toEffects action errorMessage task =
  task
  |> Task.map (always action)
  |> Task.mapError (Debug.log errorMessage)
  |> onError (always (action |> Task.succeed))
  |> Effects.task
