module FirebaseModel.Mapping where

import Dict exposing (Dict)
import Signal exposing (Address)
import Json.Decode as Decode
import Task
import Lazy exposing (Lazy)
import TaskUtil exposing (HandledTask)
import ElmFire

type alias Stored a =
  Remote (Result Error a)

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
            Signal.map
              (\state ->
                state.cache |> mapping.transform url
              )
              states,
          tasksToRun =
            Signal.mergeMany [
              subscribeTask |> Signal.constant,
              states |> Signal.map (mapping.handle eventMailbox.address url)
            ]
        }
      states =
        Signal.foldp update initialState eventMailbox.signal
      eventMailbox =
        Signal.mailbox Nothing
      subscribeTask =
        mapping.subscribe eventMailbox.address url
  in result

type alias Mapping a =
  {
    transform: String -> Cache -> Stored a,
    subscribe: Address Event -> String -> HandledTask,
    unsubscribe: String -> Cache -> HandledTask,
    handle: Address Event -> String -> State -> HandledTask
  }

type alias State =
  {
    cache: Cache,
    previousCache: Cache,
    lastEvent: Event
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
  Dict String Entry -- TODO List Entry and include location (instead of url) and mapping in Entry for disambiguation

type alias Entry =
  {
    maybeValue: Maybe Decode.Value,
    subscription: Result SubscriptionError ElmFire.Subscription
  }

initialState : State
initialState =
  {
    cache =
      Dict.empty,
    previousCache =
      Dict.empty,
    lastEvent =
      Nothing
  }

updateCache : Event -> (Cache -> Cache) -> State -> State
updateCache lastEvent transform state =
  { state |
    cache =
      state.cache |> transform,
    lastEvent =
      lastEvent
  }

update : Event -> State -> State
update event state =
  case event of
    Nothing ->
      state
    Just remoteEntryEvent ->
      case remoteEntryEvent.data of
        Subscribed subscription ->
          state |> updateCache event (\cache ->
            cache |> Dict.update remoteEntryEvent.url (\maybeEntry ->
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
          )
        Unsubscribed ->
          state |> updateCache event (\cache ->
            cache |> Dict.update remoteEntryEvent.url (\maybeEntry ->
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
          )
        ValueChanged value ->
          state |> updateCache event (\cache ->
            cache |> Dict.update remoteEntryEvent.url (\maybeEntry ->
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
          )
        _ ->
          state

-- Mappings

fromDecoder : Decode.Decoder a -> Mapping a
fromDecoder decoder =
  {
    transform =
      decode decoder,
    subscribe =
      subscribe,
    unsubscribe =
      unsubscribe,
    handle = \address url state ->
      Task.succeed ()
  }

decode : Decode.Decoder a -> String -> Cache -> Stored a
decode decoder url cache =
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
          case value |> Decode.decodeValue decoder of
            Err error ->
              Err <| DecodingError error
            Ok model ->
              Ok model
  ) |> Remote url

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

unsubscribe : String -> Cache -> HandledTask
unsubscribe url cache =
  (cache |> Dict.get url) `Maybe.andThen` (\entry ->
    entry.subscription
    |> Result.toMaybe
    |> Maybe.map (\subscription ->
      subscription
      |> ElmFire.unsubscribe
      |> TaskUtil.swallowError "ElmFire.unsubscribe failed"
    )
  ) |> TaskUtil.orDoNothing

(:=) : String -> Mapping a -> Mapping a
(:=) key mapping =
  let result =
        {
          transform = \url cache ->
            mapping.transform (realUrl url) cache,
          subscribe = \address url ->
            mapping.subscribe address (realUrl url),
          unsubscribe = \url cache ->
            mapping.unsubscribe (realUrl url) cache,
          handle = \address url state ->
            mapping.handle address url state
        }
      realUrl = \url ->
        url ++ "/" ++ key
  in result

object1 : (Stored a -> b) -> Mapping a -> Mapping b -- TODO (String, Mapping a) instead of (:=)?
object1 function mapping =
  { mapping |
    transform = \url cache ->
      mapping.transform url cache |> function |> Ok |> Remote url
  }

object2 : (Stored a -> Stored b -> c) -> Mapping a -> Mapping b -> Mapping c
object2 function mappingA mappingB =
  {
    transform = \url cache ->
      let result =
            function a b |> Ok |> Remote url
          a =
            mappingA.transform url cache
          b =
            mappingB.transform url cache
      in result,
    subscribe = \address url ->
      TaskUtil.parallel [
        mappingA.subscribe address url,
        mappingB.subscribe address url
      ],
    unsubscribe = \url cache ->
      TaskUtil.parallel [
        mappingA.unsubscribe url cache,
        mappingB.unsubscribe url cache
      ],
    handle = \address url state ->
      TaskUtil.parallel [
        mappingA.handle address url state,
        mappingB.handle address url state
      ]
  }

object3 : (Stored a -> Stored b -> Stored c -> d) -> Mapping a -> Mapping b -> Mapping c -> Mapping d
object3 function mappingA mappingB mappingC =
  {
    transform = \url cache ->
      let result =
            function a b c |> Ok |> Remote url
          a =
            mappingA.transform url cache
          b =
            mappingB.transform url cache
          c =
            mappingC.transform url cache
      in result,
    subscribe = \address url ->
      TaskUtil.parallel [
        mappingA.subscribe address url,
        mappingB.subscribe address url,
        mappingC.subscribe address url
      ],
    unsubscribe = \url cache ->
      TaskUtil.parallel [
        mappingA.unsubscribe url cache,
        mappingB.unsubscribe url cache,
        mappingC.unsubscribe url cache
      ],
    handle = \address url state ->
      TaskUtil.parallel [
        mappingA.handle address url state,
        mappingB.handle address url state,
        mappingC.handle address url state
      ]
  }

object4 : (Stored a -> Stored b -> Stored c -> Stored d -> e) -> Mapping a -> Mapping b -> Mapping c -> Mapping d -> Mapping e
object4 function mappingA mappingB mappingC mappingD =
  {
    transform = \url cache ->
      let result =
            function a b c d |> Ok |> Remote url
          a =
            mappingA.transform url cache
          b =
            mappingB.transform url cache
          c =
            mappingC.transform url cache
          d =
            mappingD.transform url cache
      in result,
    subscribe = \address url ->
      TaskUtil.parallel [
        mappingA.subscribe address url,
        mappingB.subscribe address url,
        mappingC.subscribe address url,
        mappingD.subscribe address url
      ],
    unsubscribe = \url cache ->
      TaskUtil.parallel [
        mappingA.unsubscribe url cache,
        mappingB.unsubscribe url cache,
        mappingC.unsubscribe url cache,
        mappingD.unsubscribe url cache
      ],
    handle = \address url state ->
      TaskUtil.parallel [
        mappingA.handle address url state,
        mappingB.handle address url state,
        mappingC.handle address url state,
        mappingD.handle address url state
      ]
  }

-- TODO up to object8

map : (a -> b) -> Mapping a -> Mapping b
map function mapping =
  { mapping |
    transform = \url cache ->
      mapping.transform url cache
      |> mapRemote (\data ->
        data |> Result.map function
      )
  }

mapRemote : (a -> b) -> Remote a -> Remote b
mapRemote function remote =
  { remote |
    data =
      remote.data |> function
  }

oneOf : Dict String (Mapping a) -> Mapping a
oneOf mappings =
  let result =
        {
          transform = \url cache ->
            let result =
                  storedTypeName `Result.andThen` (\typeName ->
                    (findMapping typeName) `Result.andThen` (\mapping ->
                      mapping.transform (valueUrl url) cache
                      |> .data
                    )
                  ) |> Remote url
                storedTypeName =
                  decode Decode.string (typeUrl url) cache
                  |> .data
                findMapping typeName =
                  mappings
                  |> Dict.get typeName
                  |> Result.fromMaybe (DecodingError <| "Unknown mapping: " ++ typeName)
            in result,
          subscribe = \address url ->
            TaskUtil.parallel [
              subscribe address (typeUrl url),
              subscribe address (valueUrl url)
            ],
          unsubscribe = \url cache ->
            TaskUtil.parallel [
              unsubscribe (typeUrl url) cache,
              unsubscribe (valueUrl url) cache
              -- TODO unsubscribe with current mapping
            ],
          handle = \address url state ->
            let result =
                  TaskUtil.parallel (handleCurrentMapping :: resubscribe)
                resubscribe =
                  state.lastEvent
                  |> Maybe.map (\remoteEntryEvent ->
                    if remoteEntryEvent.url == typeUrl url then
                      [
                        unsubscribeWithOldMapping,
                        subscribeWithNewMapping
                      ]
                    else
                      []
                  )
                  |> Maybe.withDefault []
                unsubscribeWithOldMapping =
                  maybeOldMapping |> Maybe.map (\oldMapping ->
                    oldMapping.unsubscribe (valueUrl url) state.cache
                  ) |> TaskUtil.orDoNothing
                subscribeWithNewMapping =
                  maybeNewMapping |> Maybe.map (\newMapping ->
                    newMapping.subscribe address (valueUrl url)
                  ) |> TaskUtil.orDoNothing
                handleCurrentMapping =
                  maybeNewMapping |> Maybe.map (\newMapping ->
                    newMapping.handle address (valueUrl url) state
                  ) |> TaskUtil.orDoNothing
                maybeOldMapping =
                  findMapping state.previousCache
                maybeNewMapping =
                  findMapping state.cache
                findMapping cache =
                  (cache |> Dict.get (typeUrl url))
                  `Maybe.andThen` .maybeValue
                  `Maybe.andThen` (\value ->
                    value
                    |> Decode.decodeValue Decode.string
                    |> Result.toMaybe
                  )
                  `Maybe.andThen` (\typeName ->
                    mappings |> Dict.get typeName
                  )
            in result
        }
      typeUrl url =
        url ++ "/type"
      valueUrl url =
        url ++ "/value"
  in result

{-
reference : Mapping a -> Mapping (Reference a)

type alias Reference a =
  {
    get: () -> Stored a
  }

list : Mapping a -> Mapping (List a)
-}
