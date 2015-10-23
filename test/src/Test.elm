module Test where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Html.Attributes as Attributes
import Effects exposing (Never)
import StartApp exposing (App)
import ElmFire exposing (Location)
import Component
import StructuralEditor.StringListEditor as StringListEditor

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

app : App Model
app =
  Component.run
    {
      init =
        StringListEditor.init actionMailbox.address location,
      update =
        StringListEditor.update actionMailbox.address,
      view =
        StringListEditor.view "," 188,
      inputs =
        inputs
    }

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test" |> ElmFire.fromUrl

inputs : List (Signal Action)
inputs =
  [actionMailbox.signal]

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox StringListEditor.None

type alias Model =
  StringListEditor.Model

type alias Action =
  StringListEditor.Action
