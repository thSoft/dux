module ElmFireSync.ValueRef where

import Task exposing (Task)
import Signal exposing (Address)
import Json.Decode as Decode
import ElmFire exposing (Reference, Location)
import Component exposing (Update)
import ElmFireSync.Codec exposing (Codec)
import ElmFireSync.Ref as Ref exposing (Ref)

type alias ValueRef data =
  Ref (Model data)

type alias Model data =
  Result NoData data

type NoData =
  Loading |
  DecodingFailed String

init : Location -> Address (Ref.Action action) -> Update (ValueRef data)
init =
  Ref.init initialModel

initialModel : Model data
initialModel =
  Err Loading

update : Codec data -> Address (Ref.Action action) -> Ref.Action action -> ValueRef data -> Update (ValueRef data)
update codec =
  Ref.update (kind codec)

kind : Codec data -> Ref.Kind (Model data) action
kind codec =
  {
    valueChanged _ snapshot _ =
      Component.return
        (snapshot.value
        |> Decode.decodeValue codec.decoder
        |> Result.formatError DecodingFailed),
    childAdded _ _ model =
      Component.return model,
    childRemoved _ _ model =
      Component.return model,
    childMoved _ _ model =
      Component.return model,
    customAction _ _ model =
      Component.return model
  }

set : Codec data -> data -> Ref (Model data) -> Task ElmFire.Error Reference
set codec value ref =
  let result =
        ElmFire.setWithPriority json priority location -- ElmFire.set would clear priority
      json =
        value |> codec.encode
      priority =
        ref |> Ref.getPriority
      location =
        ref.location
  in result
