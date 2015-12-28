module Test.Dux.Environment.NumberLiteralEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import ElmFire exposing (Location)
import Component exposing (Component, Output)
import Dux.Environment.NumberLiteralEditor as NumberLiteralEditor exposing (NumberLiteralEditor)

main : Signal Html
main =
  output.html

port tasks : Signal (Task () ())
port tasks =
  output.tasks

type alias Model =
  NumberLiteralEditor

output : Output Model
output =
  NumberLiteralEditor.component location |> Component.start

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test/NumberLiteralEditor" |> ElmFire.fromUrl
