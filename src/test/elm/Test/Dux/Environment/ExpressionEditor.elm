module Test.Dux.Environment.ExpressionEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import ElmFire exposing (Location)
import Component exposing (Component, Output)
import Dux.Environment.ExpressionEditor as ExpressionEditor exposing (ExpressionEditor)

main : Signal Html
main =
  output.html

port tasks : Signal (Task () ())
port tasks =
  output.tasks

type alias Model =
  ExpressionEditor

output : Output Model
output =
  Component.start
    {
      init =
        ExpressionEditor.init location,
      update =
        ExpressionEditor.update,
      view =
        ExpressionEditor.view True,
      inputs =
        []
    }

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test/ExpressionEditor" |> ElmFire.fromUrl
