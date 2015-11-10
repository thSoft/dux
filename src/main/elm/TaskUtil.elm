module TaskUtil where

import Signal exposing (Address)
import Task exposing (Task)
import Debug

andThen : (a -> Task x b) -> Task x a -> Task x b
andThen =
  flip Task.andThen

onError : (x -> Task y a) -> Task x a -> Task y a
onError =
  flip Task.onError

swallowError : String -> Task x a -> Task () ()
swallowError errorMessage task =
  task
  |> Task.map (always ())
  |> Task.mapError (Debug.log errorMessage)
  |> onError (always <| Task.succeed ())


{-| A convenience for a common pattern, where we want to take the result of
a task and then send it to an address with a tag. For example, if you had
the following types:

    type Action
        = HandleResult Int
        | HandleError String

    mailbox : Mailbox Action
    task : Task Int String

... then you might use `notify` in this way:

    task
        `andThen` notify mailbox.address HandleResult
        `onError` notify mailbox.address HandleError

Of course, the (result -> action) function need not actually be a tag ... it
could be any function that fits the signature.
-}
notify : Address action -> (result -> action) -> result -> Task x ()
notify address tag result =
    Signal.send address (tag result)
