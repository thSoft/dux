module StructuralEditor.StringListEditor where

import Char
import Keyboard exposing (KeyCode)
import String
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Signal exposing (Address, Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Effects exposing (Effects, Never)
import Debug
import ElmFire exposing (Location)
import Keyboard.Keys exposing (..)
import Component exposing (Update)
import TaskUtil
import ElmFireSync.Ref as Ref exposing (Ref)
import ElmFireSync.RefList as RefList exposing (RefList)
import StructuralEditor.StringEditor as StringEditor

type alias Model =
  {
    refList: RefList String,
    editors: Dict String StringEditor.Model,
    adder: StringEditor.Model,
    adderPosition: AdderPosition
  }

type Action =
  None |
  Delete (RefList.Child String) |
  SetAdderPosition AdderPosition |
  RefListAction (RefList.Action String) |
  EditorAction EditorId StringEditor.Action

type EditorId =
  Existing String |
  Adder

type AdderPosition =
  Nowhere |
  Before String |
  After String

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
                StringEditor.init "",
              adderPosition =
                Nowhere
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
    Delete child ->
      {
        model =
          model,
        effects =
          child.ref |> Ref.delete
          |> TaskUtil.toEffects None "ElmFire.remove failed"
      }
    SetAdderPosition adderPosition ->
      Component.return
        { model | adderPosition <- adderPosition }
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
                    { model |
                      adder <-
                        adderUpdate.model,
                      adderPosition <-
                        if stringEditorAction == StringEditor.Save then Nowhere else model.adderPosition
                    },
                  effects =
                    adderUpdate.effects |> Effects.map (EditorAction Adder)
                }
              adderUpdate =
                model.adder |> StringEditor.update (adderRef address model) stringEditorAction
          in result

type alias Separator =
  {
    html: Html,
    keyCode: KeyCode
  }

commaSeparator : Separator
commaSeparator =
  {
    html =
      "," |> Html.text,
    keyCode =
      188
  }

lineSeparator : Separator
lineSeparator =
  {
    html =
      Html.br
        [
          Attributes.style [
            ("line-height", "1.4")
          ]
        ]
        [],
    keyCode =
      13
  }

view : Separator -> Address Action -> Model -> Html
view separator address model =
  let result =
        Html.div
          []
          children
      children =
        if model.refList.children |> Dict.isEmpty then
          [adder address model]
        else
          model.refList.children |> Dict.toList |> List.map (\(url, child) ->
            maybeAdder (Before url) ++
            [
              transformer separator False address model url child,
              editor address model url child,
              transformer separator True address model url child
            ] ++
            maybeAdder (After url)
          ) |> List.intersperse [separator.html] |> List.concat
      maybeAdder adderPosition =
        if model.adderPosition == adderPosition then [adder address model] else []
  in result

editor : Address Action -> Model -> String -> RefList.Child String -> Html
editor address model url child =
  model.editors |> Dict.get url |> Maybe.map (\stringEditor ->
    stringEditor
    |> StringEditor.view
      child.ref
      toString
      (address |> forwardToStringEditor (Existing url))
  ) |> Maybe.withDefault ("Programming error, no editor for " ++ url |> Html.text)

transformer : Separator -> Bool -> Address Action -> Model -> String -> RefList.Child String -> Html
transformer separator after address model url child =
  let result =
        Html.span
          [
            Attributes.contenteditable True,
            StringEditor.handleKeys True [removerKey.keyCode, tab.keyCode],
            Events.onKeyUp address keyUpAction
          ]
          (if (after && model.adderPosition == After url) || (not after && model.adderPosition == Before url) then
            [separator.html]
          else [])
      keyUpAction keyCode =
        if keyCode == removerKey.keyCode then
          Delete child
        else
          if keyCode == separator.keyCode then
            SetAdderPosition adderPosition
          else
            None
      removerKey =
        if after then backspace else delete
      adderPosition =
        if after then After url else Before url
  in result

adder : Address Action -> Model -> Html
adder address model =
  model.adder
  |> StringEditor.view
    (adderRef address model)
    (always "")
    (address |> forwardToStringEditor Adder)

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
