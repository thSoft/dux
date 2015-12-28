module Test.StructuralEditor.ChoiceEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Json.Decode as Decode
import ElmFire exposing (Location)
import Component exposing (Component, Output)
import StructuralEditor.Codecs as Codecs
import StructuralEditor.StringConverters as StringConverters
import StructuralEditor.ValueEditor as ValueEditor exposing (ValueEditor)

main : Signal Html
main =
  output.html

port tasks : Signal (Task () ())
port tasks =
  output.tasks

type alias Model =
  ValueEditor Element

type Element =
  String String |
  Number Float |
  Boolean Bool

output : Output Model
output =
  Component.start
    {
      init =
        ValueEditor.init location,
      update =
        ValueEditor.update context,
      view =
        ValueEditor.view context True,
      inputs =
        []
    }

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test/ChoiceEditor" |> ElmFire.fromUrl

context : ValueEditor.Context Element
context =
  {
    codec =
      {
        decoder =
          Decode.oneOf [
            Codecs.string |> .decoder |> Decode.map String,
            Codecs.float |> .decoder |> Decode.map Number,
            Codecs.bool |> .decoder |> Decode.map Boolean
          ],
        encode = \a ->
          case a of
            String string ->
              (Codecs.string |> .encode) string
            Number float ->
              (Codecs.float |> .encode) float
            Boolean bool ->
              (Codecs.bool |> .encode) bool
      },
    stringConverter =
      {
        toString = \a ->
          case a of
            String string ->
              (StringConverters.string |> .toString) string
            Number float ->
              (StringConverters.float |> .toString) float
            Boolean bool ->
              (StringConverters.bool |> .toString) bool,
        fromString = \string ->
          ((StringConverters.string |> .fromString) string |> List.map String) ++
          ((StringConverters.float |> .fromString) string |> List.map Number) ++
          ((StringConverters.bool |> .fromString) string |> List.map Boolean)
      }
  }
