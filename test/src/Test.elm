module Test where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Never)
import StartApp exposing (App)
import Component
import StructuralEditor.StringListEditor as StringListEditor

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

type alias Model =
  StringListEditor.Model

type alias Action =
  StringListEditor.Action

app : App Model
app =
  Component.run
    {
      init =
        StringListEditor.init actionMailbox.address url,
      update =
        StringListEditor.update actionMailbox.address,
      view =
        StringListEditor.view StringListEditor.lineSeparator,
      inputs =
        inputs
    }

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox StringListEditor.None

url : String
url =
  "https://thsoft.firebaseio.com/DUX/test"

inputs : List (Signal Action)
inputs =
  [actionMailbox.signal]
