module Dux.Environment.FunctionTypeEditor where

import Signal exposing (Address)
import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import ElmFire exposing (Location)
import DecodeUtil
import Component exposing (Update)
import StructuralEditor.Editor as Editor exposing (Editor)
import StructuralEditor.ValueEditor as ValueEditor exposing (ValueEditor)

type alias FunctionTypeEditor =
  Editor Model

type alias Model =
  ValueEditor.Model Data

type Data =
  Unknown |
  Add |
  Subtract |
  Multiply |
  Divide

type alias Action =
  ValueEditor.Action

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
  {
    codec =
      {
        decoder =
          Decode.string |> Decode.map (findType >> Maybe.withDefault Unknown),
        encode a =
          findTypeName a |> Encode.string
      },
    stringConverter =
      {
        toString a =
          findTypeName a,
        fromString string =
          [string |> findType] |> List.filterMap identity
      }
  }

findType : String -> Maybe Data
findType typeName =
  types |> Dict.get typeName

findTypeName : Data -> String
findTypeName data =
  types |> Dict.toList |> List.map (\(name, functionType) ->
    if functionType == data then Just name else Nothing
  ) |> Maybe.oneOf |> Maybe.withDefault ""

types : Dict String Data
types =
  Dict.fromList [
    ("+", Add),
    ("-", Subtract),
    ("*", Multiply),
    ("/", Divide)
  ]

isValid : Decode.Value -> Bool
isValid value =
  value |> DecodeUtil.canBeDecodedWith context.codec.decoder
