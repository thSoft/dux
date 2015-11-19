module Test.StructuralEditor.StringEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import ElmFire exposing (Location)
import Component exposing (Component, Output)
import StructuralEditor.EditorKind as EditorKind
import StructuralEditor.ValueEditor as ValueEditor

main : Signal Html
main =
  output.html

port tasks : Signal (Task () ())
port tasks =
  output.tasks

type alias Element =
  String

type alias Model =
  ValueEditor.Model Element

output : Output Model
output =
  Component.start
    {
      init =
        ValueEditor.init EditorKind.string location,
      update =
        ValueEditor.update,
      view =
        ValueEditor.view,
      inputs =
        []
    }

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test/StringEditor" |> ElmFire.fromUrl
