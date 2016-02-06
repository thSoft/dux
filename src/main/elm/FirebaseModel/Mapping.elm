module FirebaseModel.Mapping where

import Dict exposing (Dict)
import Signal exposing (Address)
import Json.Decode as Decode
import Task
import Lazy exposing (Lazy)
import TaskUtil exposing (HandledTask)
import ElmFire

type alias Output a =
  {
    model: Signal (Stored a),
    tasksToRun : Signal HandledTask
  }

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

mirror : Mapping a -> String -> Output a
mirror mapping url =
  let result =
        {
          model =
            Signal.map
              (\state ->
                state.cache |> mappingFunctions.transform url
              )
              states,
          tasksToRun =
            Signal.mergeMany [
              subscribeTask |> Signal.constant,
              states |> Signal.map (mappingFunctions.handle eventMailbox.address url)
            ]
        }
      states =
        Signal.foldp update initialState eventMailbox.signal
      eventMailbox =
        Signal.mailbox Nothing
      subscribeTask =
        mappingFunctions.subscribe eventMailbox.address url
      mappingFunctions =
        mapping |> getFunctions
  in result

type Mapping a =
  Direct (MappingFunctions a) |
  Recursive (Lazy (MappingFunctions a))

type alias MappingFunctions a =
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
  ValueChanged Decode.Value

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

update : Event -> State -> State
update event state =
  case event of
    Nothing ->
      state
    Just remoteEntryEvent ->
      case remoteEntryEvent.data of
        Subscribed subscription ->
          state |> updateEntry
            event
            remoteEntryEvent.url
            FieldSubscription
            {
              maybeValue =
                Nothing,
              subscription =
                Ok subscription
            }
        Unsubscribed ->
          state |> updateEntry
            event
            remoteEntryEvent.url
            FieldSubscription
            {
              maybeValue =
                Nothing,
              subscription =
                Err NoSubscription
            }
        SubscriptionFailed error ->
          state |> updateEntry
            event
            remoteEntryEvent.url
            FieldSubscription
            {
              maybeValue =
                Nothing,
              subscription =
                Err <| ElmFireError error
            }
        Cancelled cancellation ->
          state |> updateEntry
            event
            remoteEntryEvent.url
            FieldSubscription
            {
              maybeValue =
                Nothing,
              subscription =
                Err <| ElmFireCancellation cancellation
            }
        ValueChanged value ->
          state |> updateEntry
            event
            remoteEntryEvent.url
            FieldMaybeValue
            {
              maybeValue =
                Just value,
              subscription =
                Err NoSubscription
            }

type EntryField =
  FieldMaybeValue |
  FieldSubscription

updateEntry : Event -> String -> EntryField -> Entry -> State -> State
updateEntry event url entryField newEntry state =
  state |> updateCache event (\cache ->
    cache |> Dict.update url (\maybeEntry ->
      case maybeEntry of
        Nothing ->
          Just newEntry
        Just entry ->
          Just <| case entryField of
            FieldMaybeValue ->
              { entry |
                maybeValue =
                  newEntry.maybeValue
              }
            FieldSubscription ->
              { entry |
                subscription =
                  newEntry.subscription
              }
    )
  )

updateCache : Event -> (Cache -> Cache) -> State -> State
updateCache lastEvent transform state =
  { state |
    cache =
      state.cache |> transform,
    lastEvent =
      lastEvent
  }

-- Mappings

fromDecoder : Decode.Decoder a -> Mapping a
fromDecoder decoder =
  Direct {
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
        Direct {
          transform = \url cache ->
            mappingFunctions.transform (realUrl url) cache,
          subscribe = \address url ->
            mappingFunctions.subscribe address (realUrl url),
          unsubscribe = \url cache ->
            mappingFunctions.unsubscribe (realUrl url) cache,
          handle = \address url state ->
            mappingFunctions.handle address (realUrl url) state
        }
      realUrl = \url ->
        url ++ "/" ++ key
      mappingFunctions =
        mapping |> getFunctions
  in result

object1 : (Stored a -> b) -> Mapping a -> Mapping b -- TODO (String, Mapping a) instead of (:=)?
object1 function mapping =
  let result =
        Direct { mappingFunctions |
          transform = \url cache ->
            mappingFunctions.transform url cache |> function |> Ok |> Remote url
        }
      mappingFunctions =
        mapping |> getFunctions
  in result

object2 : (Stored a -> Stored b -> c) -> Mapping a -> Mapping b -> Mapping c
object2 function mappingA mappingB =
  let result =
        Direct {
          transform = \url cache ->
            let result =
                  function a b |> Ok |> Remote url
                a =
                  mappingFunctionsA.transform url cache
                b =
                  mappingFunctionsB.transform url cache
            in result,
          subscribe = \address url ->
            TaskUtil.parallel [
              mappingFunctionsA.subscribe address url,
              mappingFunctionsB.subscribe address url
            ],
          unsubscribe = \url cache ->
            TaskUtil.parallel [
              mappingFunctionsA.unsubscribe url cache,
              mappingFunctionsB.unsubscribe url cache
            ],
          handle = \address url state ->
            TaskUtil.parallel [
              mappingFunctionsA.handle address url state,
              mappingFunctionsB.handle address url state
            ]
        }
      mappingFunctionsA =
        mappingA |> getFunctions
      mappingFunctionsB =
        mappingB |> getFunctions
  in result

object3 : (Stored a -> Stored b -> Stored c -> d) -> Mapping a -> Mapping b -> Mapping c -> Mapping d
object3 function mappingA mappingB mappingC =
  let result =
        Direct {
          transform = \url cache ->
            let result =
                  function a b c |> Ok |> Remote url
                a =
                  mappingFunctionsA.transform url cache
                b =
                  mappingFunctionsB.transform url cache
                c =
                  mappingFunctionsC.transform url cache
            in result,
          subscribe = \address url ->
            TaskUtil.parallel [
              mappingFunctionsA.subscribe address url,
              mappingFunctionsB.subscribe address url,
              mappingFunctionsC.subscribe address url
            ],
          unsubscribe = \url cache ->
            TaskUtil.parallel [
              mappingFunctionsA.unsubscribe url cache,
              mappingFunctionsB.unsubscribe url cache,
              mappingFunctionsC.unsubscribe url cache
            ],
          handle = \address url state ->
            TaskUtil.parallel [
              mappingFunctionsA.handle address url state,
              mappingFunctionsB.handle address url state,
              mappingFunctionsC.handle address url state
            ]
        }
      mappingFunctionsA =
        mappingA |> getFunctions
      mappingFunctionsB =
        mappingB |> getFunctions
      mappingFunctionsC =
        mappingC |> getFunctions
  in result

object4 : (Stored a -> Stored b -> Stored c -> Stored d -> e) -> Mapping a -> Mapping b -> Mapping c -> Mapping d -> Mapping e
object4 function mappingA mappingB mappingC mappingD =
  let result =
        Direct {
          transform = \url cache ->
            let result =
                  function a b c d |> Ok |> Remote url
                a =
                  mappingFunctionsA.transform url cache
                b =
                  mappingFunctionsB.transform url cache
                c =
                  mappingFunctionsC.transform url cache
                d =
                  mappingFunctionsD.transform url cache
            in result,
          subscribe = \address url ->
            TaskUtil.parallel [
              mappingFunctionsA.subscribe address url,
              mappingFunctionsB.subscribe address url,
              mappingFunctionsC.subscribe address url,
              mappingFunctionsD.subscribe address url
            ],
          unsubscribe = \url cache ->
            TaskUtil.parallel [
              mappingFunctionsA.unsubscribe url cache,
              mappingFunctionsB.unsubscribe url cache,
              mappingFunctionsC.unsubscribe url cache,
              mappingFunctionsD.unsubscribe url cache
            ],
          handle = \address url state ->
            TaskUtil.parallel [
              mappingFunctionsA.handle address url state,
              mappingFunctionsB.handle address url state,
              mappingFunctionsC.handle address url state,
              mappingFunctionsD.handle address url state
            ]
        }
      mappingFunctionsA =
        mappingA |> getFunctions
      mappingFunctionsB =
        mappingB |> getFunctions
      mappingFunctionsC =
        mappingC |> getFunctions
      mappingFunctionsD =
        mappingD |> getFunctions
  in result

-- TODO up to object8

map : (a -> b) -> Mapping a -> Mapping b
map function mapping =
  let result =
        Direct { mappingFunctions |
          transform = \url cache ->
            mappingFunctions.transform url cache
            |> mapRemote (\data ->
              data |> Result.map function
            )
        }
      mappingFunctions =
        mapping |> getFunctions
  in result

mapRemote : (a -> b) -> Remote a -> Remote b
mapRemote function remote =
  { remote |
    data =
      remote.data |> function
  }

oneOf : Dict String (Mapping a) -> Mapping a
oneOf mappings =
  let result =
        Direct {
          transform = \url cache ->
            let result =
                  storedTypeName `Result.andThen` (\typeName ->
                    (findMapping typeName) `Result.andThen` (\mapping ->
                      (mapping |> getFunctions).transform (valueUrl url) cache
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
            subscribe address (typeUrl url),
          unsubscribe = \url cache ->
            TaskUtil.parallel [
              unsubscribe (typeUrl url) cache,
              findMapping cache url |> Maybe.map (\mapping ->
                (mapping |> getFunctions).unsubscribe (valueUrl url) cache
              ) |> TaskUtil.orDoNothing
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
                    (oldMapping |> getFunctions).unsubscribe (valueUrl url) state.cache
                  ) |> TaskUtil.orDoNothing
                subscribeWithNewMapping =
                  maybeNewMapping |> Maybe.map (\newMapping ->
                    (newMapping |> getFunctions).subscribe address (valueUrl url)
                  ) |> TaskUtil.orDoNothing
                handleCurrentMapping =
                  maybeNewMapping |> Maybe.map (\newMapping ->
                    (newMapping |> getFunctions).handle address (valueUrl url) state
                  ) |> TaskUtil.orDoNothing
                maybeOldMapping =
                  findMapping state.previousCache url
                maybeNewMapping =
                  findMapping state.cache url
            in result
        }
      typeUrl url =
        url ++ "/type"
      valueUrl url =
        url ++ "/value"
      findMapping cache url =
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

recursive : (() -> Mapping a) -> Mapping a
recursive getMapping =
  Recursive <| Lazy.lazy (\() ->
    getMapping () |> getFunctions
  )

getFunctions : Mapping a -> MappingFunctions a
getFunctions mapping =
  case mapping of
    Direct mappingFunctions ->
      mappingFunctions
    Recursive getMappingFunctions ->
      Lazy.force getMappingFunctions

{-
reference : Mapping a -> Mapping (Reference a)

type alias Reference a =
  {
    get: () -> Stored a
  }

list : Mapping a -> Mapping (List a)
-}
