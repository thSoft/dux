module Test.StructuralEditor.StringEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Never)
import StartApp exposing (App)
import Component
import ElmFireSync.Codec as Codec
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
  Component.run
    {
      init =
        ValueEditor.init EditorKind.string url actionMailbox.address,
      update =
        ValueEditor.update,
      view =
        ValueEditor.view,
      inputs =
        inputs
    }

url : String
url =
  "https://thsoft.firebaseio.com/DUX/test/StringEditor"

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox ValueEditor.None

inputs : List (Signal Action)
inputs =
  [actionMailbox.signal]
