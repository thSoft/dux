module Test.StructuralEditor.StringEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Never)
import StartApp exposing (App)
import ElmFire exposing (Location)
import Component
import StructuralEditor.EditorKind as EditorKind
import StructuralEditor.ValueEditor as ValueEditor

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

type alias Element =
  String

type alias Model =
  ValueEditor.Model Element

type alias Action =
  ValueEditor.Action Element

app : App Model
app =
  Component.start
    {
      init =
        ValueEditor.init EditorKind.string location actionMailbox.address,
      update =
        ValueEditor.update,
      view =
        ValueEditor.view,
      inputs =
        inputs
    }

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test/StringEditor" |> ElmFire.fromUrl

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox ValueEditor.None

inputs : List (Signal Action)
inputs =
  [actionMailbox.signal]
