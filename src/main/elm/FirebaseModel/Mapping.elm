module FirebaseModel.Mapping where

import Dict exposing (Dict)
import Signal exposing (Address)
import Json.Decode as Decode
import Task
import TaskUtil
import ElmFire
import Component exposing (Update, HandledTask)

type alias Stored a =
  Result (Remote Error) a

type alias Remote a =
  {
    url: String,
    data: a
  }

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
            Signal.map (mapping.transform realUrl) caches,
          tasksToRun =
            Signal.mergeMany [
              subscribeTask |> Signal.constant,
              eventMailbox.signal |> Signal.map mapping.handle
            ]
        }
      caches =
        Signal.foldp update initialCache eventMailbox.signal
      eventMailbox =
        Signal.mailbox Nothing
      subscribeTask =
        mapping.subscribe eventMailbox.address realUrl
      realUrl =
        mapping.getRealUrl url
  in result

type alias Mapping a =
  {
    getRealUrl: String -> String,
    transform: String -> Cache -> Stored a,
    subscribe: Address Event -> String -> HandledTask,
    handle: Event -> HandledTask
  }

type alias Event =
  Maybe (Remote EntryEvent)

type EntryEvent =
  Subscribed ElmFire.Subscription |
  Unsubscribed |
  SubscriptionFailed ElmFire.Error |
  Cancelled ElmFire.Cancellation |
  ValueChanged Decode.Value |
  ChildAdded String |
  ChildRemoved String

type alias Cache =
  Dict String Entry -- TODO List Entry and include location (instead of url) and mapping in Entry

type alias Entry =
  {
    maybeValue: Maybe Decode.Value,
    subscription: Result SubscriptionError ElmFire.Subscription
  }

initialCache : Cache
initialCache =
  Dict.empty

update : Event -> Cache -> Cache
update event cache =
  case event of
    Nothing ->
      cache
    Just { url, data } ->
      case data of
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

-- Mappings

fromDecoder : Decode.Decoder a -> Mapping a
fromDecoder decoder =
  {
    getRealUrl =
      identity,
    transform =
      decode decoder,
    subscribe =
      subscribe,
    handle = \_ ->
      Task.succeed ()
  }

decode : Decode.Decoder a -> String -> Cache -> Stored a
decode decoder url cache =
  mapValue
    (\value ->
      case value |> Decode.decodeValue decoder of
        Err error ->
          Err <| DecodingError error
        Ok model ->
          Ok model
    )
    url
    cache

mapValue : (Decode.Value -> Result Error a) -> String -> Cache -> Stored a
mapValue function url cache =
  (case cache |> Dict.get url of
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
          function value
  ) |> wrap url

wrap : String -> Result Error a -> Stored a
wrap url result =
  result |> Result.formatError (\error ->
    {
      url =
        url,
      data =
        error
    }
  )

unwrap : Stored a -> Result Error a
unwrap stored =
  stored |> Result.formatError .data

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
          data =
            entryEvent
        }
        |> Signal.send address
      snapshotEvent snapshot =
        ValueChanged snapshot.value
      query =
        ElmFire.valueChanged
  in result

(:=) : String -> Mapping a -> Mapping a
(:=) key mapping =
  { mapping |
    getRealUrl = \url ->
      mapping.getRealUrl (url ++ "/" ++ key)
  }

object1 : (Stored a -> b) -> Mapping a -> Mapping b
object1 function mapping =
  { mapping |
    transform = \url cache ->
      mapping.transform url cache |> function |> Ok
  }

object2 : (Stored a -> Stored b -> c) -> Mapping a -> Mapping b -> Mapping c
object2 function mappingA mappingB =
  {
    getRealUrl =
      identity,
    transform = \url cache ->
      let result =
            function a b |> Ok
          a =
            mappingA.transform (mappingA.getRealUrl url) cache
          b =
            mappingB.transform (mappingB.getRealUrl url) cache
      in result,
    subscribe = \address url ->
      TaskUtil.parallel [
        mappingA.subscribe address (mappingA.getRealUrl url),
        mappingB.subscribe address (mappingB.getRealUrl url)
      ],
    handle = \event ->
      TaskUtil.parallel [
        mappingA.handle event,
        mappingB.handle event
      ]
  }

object3 : (Stored a -> Stored b -> Stored c -> d) -> Mapping a -> Mapping b -> Mapping c -> Mapping d
object3 function mappingA mappingB mappingC =
  {
    getRealUrl =
      identity,
    transform = \url cache ->
      let result =
            function a b c |> Ok
          a =
            mappingA.transform (mappingA.getRealUrl url) cache
          b =
            mappingB.transform (mappingB.getRealUrl url) cache
          c =
            mappingC.transform (mappingC.getRealUrl url) cache
      in result,
    subscribe = \address url ->
      TaskUtil.parallel [
        mappingA.subscribe address (mappingA.getRealUrl url),
        mappingB.subscribe address (mappingB.getRealUrl url),
        mappingC.subscribe address (mappingC.getRealUrl url)
      ],
    handle = \event ->
      TaskUtil.parallel [
        mappingA.handle event,
        mappingB.handle event,
        mappingC.handle event
      ]
  }

object4 : (Stored a -> Stored b -> Stored c -> Stored d -> e) -> Mapping a -> Mapping b -> Mapping c -> Mapping d -> Mapping e
object4 function mappingA mappingB mappingC mappingD =
  {
    getRealUrl =
      identity,
    transform = \url cache ->
      let result =
            function a b c d |> Ok
          a =
            mappingA.transform (mappingA.getRealUrl url) cache
          b =
            mappingB.transform (mappingB.getRealUrl url) cache
          c =
            mappingC.transform (mappingC.getRealUrl url) cache
          d =
            mappingD.transform (mappingD.getRealUrl url) cache
      in result,
    subscribe = \address url ->
      TaskUtil.parallel [
        mappingA.subscribe address (mappingA.getRealUrl url),
        mappingB.subscribe address (mappingB.getRealUrl url),
        mappingC.subscribe address (mappingC.getRealUrl url),
        mappingD.subscribe address (mappingD.getRealUrl url)
      ],
    handle = \event ->
      TaskUtil.parallel [
        mappingA.handle event,
        mappingB.handle event,
        mappingC.handle event,
        mappingD.handle event
      ]
  }

-- TODO up to object8

map : (a -> b) -> Mapping a -> Mapping b
map function mapping =
  { mapping |
    transform = \url cache ->
      mapping.transform url cache |> Result.map function
  }

oneOf : Dict String (Mapping a) -> Mapping a
oneOf mappings =
  let result =
        {
          getRealUrl =
            identity,
          transform = \url cache ->
            let result =
                  storedTypeName `Result.andThen` (\typeName ->
                    (findMapping typeName) `Result.andThen` (\mapping ->
                      mapping.transform (url |> mapping.getRealUrl |> valueUrl) cache
                      |> unwrap
                    )
                  ) |> wrap url
                storedTypeName =
                  decode Decode.string (typeUrl url) cache
                  |> unwrap
            in result,
          subscribe = \address url ->
            TaskUtil.parallel [
              subscribe address (typeUrl url),
              subscribe address (valueUrl url)
            ],
          handle = \_ ->
            Task.succeed () -- TODO handle type change and call handle of current mapping
        }
      typeUrl url =
        url ++ "/type"
      valueUrl url =
        url ++ "/value"
      findMapping typeName =
        mappings
        |> Dict.get typeName
        |> Result.fromMaybe (DecodingError <| "Unknown mapping: " ++ typeName)
  in result

{-
reference : Mapping a -> Mapping (Reference a)

type alias Reference a =
  {
    get: () -> Stored a
  }

list : Mapping a -> Mapping (List a)
-}
