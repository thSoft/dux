module Test.Dux.Environment.FunctionTypeEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import ElmFire exposing (Location)
import Component exposing (Component, Output)
import Dux.Environment.FunctionTypeEditor as FunctionTypeEditor exposing (FunctionTypeEditor)

main : Signal Html
main =
  output.html

port tasks : Signal (Task () ())
port tasks =
  output.tasks

type alias Model =
  FunctionTypeEditor

output : Output Model
output =
  FunctionTypeEditor.component location |> Component.start

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test/FunctionTypeEditor" |> ElmFire.fromUrl
