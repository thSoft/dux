module Test.Dux.Environment.FunctionCallEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import ElmFire exposing (Location)
import Component exposing (Component, Output)
import Dux.Environment.ExpressionEditor as ExpressionEditor exposing (FunctionCallEditor)

main : Signal Html
main =
  output.html

port tasks : Signal (Task () ())
port tasks =
  output.tasks

type alias Model =
  FunctionCallEditor

output : Output Model
output =
  ExpressionEditor.functionCallEditorComponent location |> Component.start

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test/FunctionCallEditor" |> ElmFire.fromUrl
