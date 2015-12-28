module Dux.Environment.NumberLiteralEditor where

import Signal exposing (Address)
import Html exposing (Html)
import Json.Decode as Decode
import ElmFire exposing (Location)
import Component exposing (Component, Update)
import DecodeUtil
import StructuralEditor.Editor as Editor exposing (Editor)
import StructuralEditor.ValueEditor as ValueEditor exposing (ValueEditor)
import StructuralEditor.ValueEditorContexts as ValueEditorContexts

type alias NumberLiteralEditor =
  Editor Model

type alias Model =
  ValueEditor.Model Data

type alias Data =
  Float

type alias Action =
  ValueEditor.Action

component : Location -> Component NumberLiteralEditor (Editor.Action Action)
component location =
  {
    init =
      init location,
    update =
      update,
    view =
      view True,
    inputs =
      []
  }

init : Location -> Address (Editor.Action Action) -> Update (Editor Model)
init location address =
  ValueEditor.init location address

update : Address (Editor.Action Action) -> Editor.Action Action -> Editor Model -> Update (Editor Model)
update address action model =
  ValueEditor.update context address action model

view : Bool -> Address (Editor.Action Action) -> Editor Model -> Html
view focused address editor =
  ValueEditor.view context focused address editor

context : ValueEditor.Context Data
context =
  ValueEditorContexts.float

isValid : Decode.Value -> Bool
isValid value =
  value |> DecodeUtil.canBeDecodedWith context.codec.decoder
