module Test.StructuralEditor.StringEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Never)
import StartApp exposing (App)
import Component
import ElmFireSync.Codec as Codec
import StructuralEditor.StringEditor as StringEditor

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

type alias Model =
  StringEditor.Model

type alias Action =
  StringEditor.Action

app : App Model
app =
  Component.run
    {
      init =
        StringEditor.init url actionMailbox.address,
      update =
        StringEditor.update,
      view =
        StringEditor.view,
      inputs =
        inputs
    }

url : String
url =
  "https://thsoft.firebaseio.com/DUX/test/StringEditor"

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox StringEditor.None

inputs : List (Signal Action)
inputs =
  [actionMailbox.signal]
