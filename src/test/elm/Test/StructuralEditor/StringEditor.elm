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

type alias Model =
  ValueEditor Element

type alias Element =
  String

output : Output Model
output =
  Component.start
    {
      init =
        ValueEditor.init location,
      update =
        ValueEditor.update context,
      view =
        ValueEditor.view True context,
      inputs =
        []
    }

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test/StringEditor" |> ElmFire.fromUrl

context : ValueEditor.Context Element
context =
  ValueEditorContexts.string
