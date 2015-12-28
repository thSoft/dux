module Test.StructuralEditor.StringEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import ElmFire exposing (Location)
import Component exposing (Component, Output)
import StructuralEditor.ValueEditor as ValueEditor exposing (ValueEditor)
import StructuralEditor.ValueEditorContexts as ValueEditorContexts

main : Signal Html
main =
  output.html

port tasks : Signal (Task () ())
port tasks =
  output.tasks

type alias Element =
  String

output : Output (ValueEditor Element)
output =
  ValueEditor.component context location |> Component.start

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test/StringEditor" |> ElmFire.fromUrl

context : ValueEditor.Context Element
context =
  ValueEditorContexts.string
