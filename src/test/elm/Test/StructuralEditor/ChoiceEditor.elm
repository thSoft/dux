module Test.StructuralEditor.ChoiceEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Json.Decode as Decode
import ElmFire exposing (Location)
import Component exposing (Component, Output)
import ElmFireSync.Codec as Codec exposing (Codec)
import StructuralEditor.EditorKind as EditorKind exposing (EditorKind)
import StructuralEditor.StringConverter as StringConverter exposing (StringConverter)
import StructuralEditor.ValueEditor as ValueEditor

main : Signal Html
main =
  output.html

port tasks : Signal (Task () ())
port tasks =
  output.tasks

type Element =
  String String |
  Number Float |
  Boolean Bool

type alias Model =
  ValueEditor.Model Element

output : Output Model
output =
  Component.start
    {
      init =
        ValueEditor.init editorKind location,
      update =
        always ValueEditor.update,
      view =
        ValueEditor.view,
      inputs =
        []
    }

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test/ChoiceEditor" |> ElmFire.fromUrl

editorKind : EditorKind Element
editorKind =
  {
    codec =
      {
        decoder =
          Decode.oneOf [
            Codec.string |> .decoder |> Decode.map String,
            Codec.float |> .decoder |> Decode.map Number,
            Codec.bool |> .decoder |> Decode.map Boolean
          ],
        encode a =
          case a of
            String string ->
              (Codec.string |> .encode) string
            Number float ->
              (Codec.float |> .encode) float
            Boolean bool ->
              (Codec.bool |> .encode) bool
      },
    stringConverter =
      {
        toString a =
          case a of
            String string ->
              (StringConverter.string |> .toString) string
            Number float ->
              (StringConverter.float |> .toString) float
            Boolean bool ->
              (StringConverter.bool |> .toString) bool,
        fromString string =
          ((StringConverter.string |> .fromString) string |> List.map String) ++
          ((StringConverter.float |> .fromString) string |> List.map Number) ++
          ((StringConverter.bool |> .fromString) string |> List.map Boolean)
      }
  }
