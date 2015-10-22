module Dux.Editor.StringListEditor where

import Dict exposing (Dict)
import Json.Decode as Decode
import Signal exposing (Address)
import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Effects, Never)
import Debug
import ElmFire exposing (Location)
import Component exposing (Update)
import ElmFireSync.RefList as RefList
import Dux.Editor.StringEditor as StringEditor

type alias Model =
  {
    refList: RefList.Model String,
    stringEditors: Dict String StringEditor.Model
  }

type Action =
  None |
  RefListAction (RefList.Action String) |
  StringEditorAction String StringEditor.Action

init : Address Action -> Location -> Update Model Action
init address location =
  let result =
        {
          model =
            {
              refList =
                refList.model,
              stringEditors =
                Dict.empty
            },
          effects =
            refList.effects |> Effects.map RefListAction
        }
      refList =
        RefList.init (address |> forwardToRefList) StringEditor.stringHandler location
  in result

update : Address Action -> Action -> Model -> Update Model Action
update address action model =
  case action of
    None ->
      Component.return model
    RefListAction refListAction ->
      let updateResult =
            {
              model =
                { model |
                  refList <- refListUpdate.model,
                  stringEditors <- updatedStringEditors },
              effects =
                refListUpdate.effects |> Effects.map RefListAction
            }
          refListUpdate =
            RefList.update (address |> forwardToRefList) refListAction model.refList
          updatedStringEditors =
            case refListAction of
              RefList.ChildAdded snapshot ->
                let result =
                      model.stringEditors |> Dict.insert (snapshot.reference |> ElmFire.toUrl) stringEditor
                    stringEditor =
                      snapshot.value
                      |> Decode.decodeValue (StringEditor.stringHandler |> .decoder)
                      |> Result.toMaybe
                      |> Maybe.withDefault ""
                      |> StringEditor.init
                in result
              RefList.ChildRemoved snapshot ->
                model.stringEditors |> Dict.remove (snapshot.reference |> ElmFire.toUrl)
              _ ->
                model.stringEditors
      in updateResult
    StringEditorAction url stringEditorAction ->
      (model.stringEditors |> Dict.get url) `Maybe.andThen` (\stringEditor ->
        model.refList.children |> Dict.get url |> Maybe.map (\child ->
          let result =
                {
                  model =
                    { model | stringEditors <- updatedStringEditors },
                  effects =
                    stringEditorUpdate.effects |> Effects.map (StringEditorAction url)
                }
              updatedStringEditors =
                model.stringEditors |> Dict.insert url stringEditorUpdate.model
              stringEditorUpdate =
                stringEditor |> StringEditor.update child.ref stringEditorAction
          in result
        )
      ) |> Maybe.withDefault (Component.return model)

view : Address Action -> Model -> Html
view address model =
  Html.div
    []
    (model.refList.children |> Dict.toList |> List.map (\(url, child) ->
      model.stringEditors |> Dict.get url |> Maybe.map (\stringEditor ->
        StringEditor.view child.ref (address |> forwardToStringEditor url) stringEditor
      ) |> Maybe.withDefault ("Programming error, no StringEditor for " ++ url |> Html.text)
    ))

forwardToRefList : Address Action -> Address (RefList.Action String)
forwardToRefList address =
  Signal.forwardTo address RefListAction

forwardToStringEditor : String -> Address Action -> Address StringEditor.Action
forwardToStringEditor url address =
  Signal.forwardTo address (StringEditorAction url)
