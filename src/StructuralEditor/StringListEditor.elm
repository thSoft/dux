module StructuralEditor.StringListEditor where

import Dict exposing (Dict)
import Json.Decode as Decode
import Signal exposing (Address, Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Html.Attributes as Attributes
import Effects exposing (Effects, Never)
import Debug
import ElmFire exposing (Location)
import Component exposing (Update)
import ElmFireSync.Ref as Ref exposing (Ref)
import ElmFireSync.RefList as RefList exposing (RefList)
import StructuralEditor.StringEditor as StringEditor

type alias Model =
  {
    refList: RefList String,
    editors: Dict String StringEditor.Model,
    adder: StringEditor.Model
  }

type Action =
  None |
  RefListAction (RefList.Action String) |
  EditorAction EditorId StringEditor.Action

type EditorId =
  Existing String |
  Adder

init : Address Action -> Location -> Update Model Action
init address location =
  let result =
        {
          model =
            {
              refList =
                refList.model,
              editors =
                Dict.empty,
              adder =
                StringEditor.init ""
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
                  editors <- updatedStringEditors },
              effects =
                refListUpdate.effects |> Effects.map RefListAction
            }
          refListUpdate =
            RefList.update (address |> forwardToRefList) refListAction model.refList
          updatedStringEditors =
            case refListAction of
              RefList.ChildAdded snapshot ->
                let result =
                      model.editors |> Dict.insert (snapshot.reference |> ElmFire.toUrl) stringEditor
                    stringEditor =
                      snapshot.value
                      |> Decode.decodeValue (StringEditor.stringHandler |> .decoder)
                      |> Result.toMaybe
                      |> Maybe.withDefault ""
                      |> StringEditor.init
                in result
              RefList.ChildRemoved snapshot ->
                model.editors |> Dict.remove (snapshot.reference |> ElmFire.toUrl)
              _ ->
                model.editors
      in updateResult
    EditorAction id stringEditorAction ->
      case id of
        Existing url ->
          (model.editors |> Dict.get url) `Maybe.andThen` (\stringEditor ->
            model.refList.children |> Dict.get url |> Maybe.map (\child ->
              let result =
                    {
                      model =
                        { model | editors <- updatedStringEditors },
                      effects =
                        stringEditorUpdate.effects |> Effects.map (EditorAction (Existing url))
                    }
                  updatedStringEditors =
                    model.editors |> Dict.insert url stringEditorUpdate.model
                  stringEditorUpdate =
                    stringEditor |> StringEditor.update child.ref stringEditorAction
              in result
            )
          ) |> Maybe.withDefault (Component.return model)
        Adder ->
          let result =
                {
                  model =
                    { model | adder <- adderUpdate.model },
                  effects =
                    adderUpdate.effects |> Effects.map (EditorAction Adder)
                }
              adderUpdate =
                model.adder |> StringEditor.update (adderRef address model) stringEditorAction
          in result

view : Address Action -> Model -> Html
view address model =
  let result =
        Html.div
          []
          (children ++ [adder])
      children =
        model.refList.children |> Dict.toList |> List.map (\(url, child) ->
          model.editors |> Dict.get url |> Maybe.map (\stringEditor ->
            stringEditor
            |> StringEditor.view
              child.ref
              toString
              (address |> forwardToStringEditor (Existing url))
          ) |> Maybe.withDefault ("Programming error, no editor for " ++ url |> Html.text)
        )
      adder =
        model.adder
        |> StringEditor.view
          (adderRef address model)
          (always "")
          (address |> forwardToStringEditor Adder)
  in result

adderRef : Address Action -> Model -> Ref String
adderRef address model =
  Ref.init
    adderRefActionMailbox.address -- dummy
    StringEditor.stringHandler
    (model.refList.location |> ElmFire.push)
  |> .model

adderRefActionMailbox : Mailbox (Ref.Action String)
adderRefActionMailbox =
  Signal.mailbox Ref.None

forwardToRefList : Address Action -> Address (RefList.Action String)
forwardToRefList address =
  Signal.forwardTo address RefListAction

forwardToStringEditor : EditorId -> Address Action -> Address StringEditor.Action
forwardToStringEditor id address =
  Signal.forwardTo address (EditorAction id)
