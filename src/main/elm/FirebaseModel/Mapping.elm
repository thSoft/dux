module FirebaseModel.Mapping where

import Dict exposing (Dict)
import Signal exposing (Address)
import Json.Decode as Decode
import ElmFire
import TaskUtil
import Component exposing (Update, HandledTask)

type alias Stored a =
  Result Error a

type Error =
  Loading |
  DecodingError String |
  SubscriptionError SubscriptionError

type SubscriptionError =
  NoSubscription |
  ElmFireError ElmFire.Error |
  ElmFireCancellation ElmFire.Cancellation

type alias Output a =
  {
    model: Signal (Stored a),
    tasksToRun : Signal HandledTask
  }

mirror : Mapping a -> String -> Output a
mirror mapping url =
  let result =
        {
          model =
            Signal.map (transformCache mapping url) caches,
          tasksToRun =
            Signal.mergeMany [
              subscribeTask |> Signal.constant
              --getTasks mapping eventMailbox
            ]
        }
      caches =
        Signal.foldp update initialCache eventMailbox.signal
      eventMailbox =
        Signal.mailbox Nothing
      subscribeTask =
        subscribe eventMailbox.address url
  in result

type alias Mapping a =
  {
    decoder: Decode.Decoder a
  }

type alias Event =
  Maybe {
    url: String,
    entryEvent: EntryEvent
  }

type EntryEvent =
  Subscribed ElmFire.Subscription |
  Unsubscribed |
  SubscriptionFailed ElmFire.Error |
  Cancelled ElmFire.Cancellation |
  ValueChanged Decode.Value{- |
  ChildAdded String |
  ChildRemoved String-}

type alias Cache =
  Dict String Entry

type alias Entry =
  {
    maybeValue: Maybe Decode.Value,
    subscription: Result SubscriptionError ElmFire.Subscription
  }

transformCache : Mapping a -> String -> Cache -> Stored a
transformCache mapping url cache =
  case cache |> Dict.get url of
    Nothing ->
      Err (SubscriptionError NoSubscription)
    Just entry ->
      case entry.maybeValue of
        Nothing ->
          case entry.subscription of
            Err subscriptionError ->
              Err (SubscriptionError subscriptionError)
            Ok _ ->
              Err Loading
        Just value ->
          case value |> Decode.decodeValue mapping.decoder of
            Err error ->
              Err <| DecodingError error
            Ok model ->
              Ok model

initialCache : Cache
initialCache =
  Dict.empty

update : Event -> Cache -> Cache
update event cache =
  case event of
    Nothing ->
      cache
    Just { url, entryEvent } ->
      case entryEvent of
        Subscribed subscription ->
          cache |> Dict.update url (\maybeEntry ->
            case maybeEntry of
              Nothing ->
                Just {
                  maybeValue =
                    Nothing,
                  subscription =
                    Ok subscription
                }
              Just entry ->
                Just { entry |
                    subscription =
                      Ok subscription
                }
          )
        Unsubscribed ->
          cache |> Dict.update url (\maybeEntry ->
            case maybeEntry of
              Nothing ->
                Just {
                  maybeValue =
                    Nothing,
                  subscription =
                    Err NoSubscription
                }
              Just entry ->
                Just { entry |
                    subscription =
                      Err NoSubscription
                }
          )
        ValueChanged value ->
          cache |> Dict.update url (\maybeEntry ->
            case maybeEntry of
              Nothing ->
                Just {
                  maybeValue =
                    Just value,
                  subscription =
                    Err NoSubscription
                }
              Just entry ->
                Just { entry |
                    maybeValue =
                      Just value
                }
          )
        _ ->
          cache

subscribe : Address Event -> String -> HandledTask
subscribe address url =
  let result =
        task
        |> TaskUtil.andThen (\subscription ->
          Subscribed subscription
          |> sendEvent
        )
        |> TaskUtil.onError (\error ->
          SubscriptionFailed error
          |> sendEvent
        )
      task =
        ElmFire.subscribe
          (\snapshot ->
            snapshotEvent snapshot
            |> sendEvent
          )
          (\cancellation ->
            Cancelled cancellation
            |> sendEvent
          )
          (query ElmFire.noOrder)
          (ElmFire.fromUrl url)
      sendEvent entryEvent =
        Just {
          url =
            url,
          entryEvent =
            entryEvent
        }
        |> Signal.send address
      snapshotEvent snapshot =
        ValueChanged snapshot.value
      query =
        ElmFire.valueChanged
  in result

--getTasks : Mapping a -> Mailbox Event -> Signal Event
--getTasks mapping eventMailbox =

-- Mappings

fromDecoder : Decode.Decoder a -> Mapping a
fromDecoder decoder =
  {
    decoder =
      decoder
  }

{-
map : (a -> b) -> Mapping a -> Mapping b

object1 : (a -> b) -> Mapping a -> Mapping b

object2 : (a -> b -> c) -> Mapping a -> Mapping b -> Mapping c

object3 : (a -> b -> c -> d) -> Mapping a -> Mapping b -> Mapping c -> Mapping d

object4 : (a -> b -> c -> d -> e) -> Mapping a -> Mapping b -> Mapping c -> Mapping d -> Mapping e

(:=) : String -> Mapping a -> Mapping a

list : Mapping a -> Mapping (List a)

oneOf : List (Mapping a) -> Mapping a -- TODO is this good?

reference : Mapping a -> Mapping (Reference a)
-}

type alias Reference a =
  {
    get: () -> Stored a
  }
