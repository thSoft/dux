module FirebaseModel.Mapping
  (
    Output, Stored, Remote, Error(..), SubscriptionError(..), InternalMapping, Mapping, Field,
    mirror, set, delete,
    fromCodec, object, withField, choice, withOption, recursive
  ) where

import Dict exposing (Dict)
import Signal exposing (Address)
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
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

mirror : InternalMapping model parent -> String -> Output model
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

set : InternalMapping model parent -> parent -> String -> Task ElmFire.Error ()
set mapping model url =
  (mapping |> getFunctions).set model url

delete : String -> Task ElmFire.Error ()
delete url =
  url |> ElmFire.fromUrl |> ElmFire.remove |> Task.map (always ())

-- TODO push : Mapping (Many a) -> a -> String -> Task ElmFire.Error ()

type alias Mapping model =
  InternalMapping model model

type InternalMapping model parent =
  Direct (MappingFunctions model parent) |
  Recursive (Lazy (MappingFunctions model parent))

type alias MappingFunctions model parent =
  {
    transform: String -> Cache -> Stored model,
    subscribe: Address Event -> String -> HandledTask,
    unsubscribe: String -> Cache -> HandledTask,
    handle: Address Event -> String -> State -> HandledTask,
    set: parent -> String -> Task ElmFire.Error ()
  }

getFunctions : InternalMapping model parent -> MappingFunctions model parent
getFunctions mapping =
  case mapping of
    Direct mappingFunctions ->
      mappingFunctions
    Recursive getMappingFunctions ->
      Lazy.force getMappingFunctions

type alias State =
  {
    cache: Cache,
    previousCache: Cache,
    lastEvent: Event
  }

type alias Cache =
  Dict String Entry -- TODO List Entry and include location (instead of url) and mapping in Entry for unique identification?

type alias Entry =
  {
    maybeValue: Maybe Decode.Value,
    subscription: Result SubscriptionError ElmFire.Subscription
  }

type alias Event =
  Maybe (Remote EntryEvent)

type EntryEvent =
  Subscribed ElmFire.Subscription |
  Unsubscribed |
  SubscriptionFailed ElmFire.Error |
  Cancelled ElmFire.Cancellation |
  ValueChanged Decode.Value

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
            (always {
              maybeValue =
                Nothing,
              subscription =
                Ok subscription
            })
        Unsubscribed ->
          state |> updateEntry
            event
            remoteEntryEvent.url
            FieldSubscription
            (always {
              maybeValue =
                Nothing,
              subscription =
                Err NoSubscription
            })
        SubscriptionFailed error ->
          state |> updateEntry
            event
            remoteEntryEvent.url
            FieldSubscription
            (always {
              maybeValue =
                Nothing,
              subscription =
                Err <| ElmFireError error
            })
        Cancelled cancellation ->
          state |> updateEntry
            event
            remoteEntryEvent.url
            FieldSubscription
            (always {
              maybeValue =
                Nothing,
              subscription =
                Err <| ElmFireCancellation cancellation
            })
        ValueChanged value ->
          state |> updateEntry
            event
            remoteEntryEvent.url
            FieldMaybeValue
            (always {
              maybeValue =
                Just value,
              subscription =
                Err NoSubscription
            })

type EntryField =
  FieldMaybeValue |
  FieldSubscription

updateEntry : Event -> String -> EntryField -> (Maybe Entry -> Entry) -> State -> State
updateEntry event url entryField getNewEntry state =
  state |> updateCache event (\cache ->
    cache |> Dict.update url (\maybeEntry ->
      let result =
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
          newEntry =
            getNewEntry maybeEntry
      in result
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

fromCodec : (model -> Encode.Value) -> Decode.Decoder model -> Mapping model
fromCodec encoder decoder =
  Direct {
    transform =
      decode decoder,
    subscribe =
      subscribe,
    unsubscribe =
      unsubscribe,
    handle = \address url state ->
      Task.succeed (),
    set = \model url ->
      (url |> ElmFire.fromUrl)
      |> ElmFire.set (model |> encoder)
      |> Task.map (always ())
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
            ValueChanged snapshot.value
            |> sendEvent
          )
          (\cancellation ->
            Cancelled cancellation
            |> sendEvent
          )
          (ElmFire.valueChanged ElmFire.noOrder)
          (ElmFire.fromUrl url)
      sendEvent entryEvent =
        Just {
          url =
            url,
          data =
            entryEvent
        }
        |> Signal.send address
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

object : constructor -> InternalMapping constructor object
object constructor =
  Direct {
    transform = \url cache ->
      constructor |> Ok |> Remote url,
    subscribe = \address url ->
      Task.succeed (),
    unsubscribe = \url cache ->
      Task.succeed (),
    handle = \address url state ->
      Task.succeed (),
    set = \model url ->
      Task.succeed ()
  }

type alias Field field object =
  {
    key: String,
    get: object -> Stored field,
    mapping: Mapping field
  }

withField : InternalMapping (Stored field -> result) object -> Field field object -> InternalMapping result object
withField functionMapping field =
  let result =
        Recursive <| Lazy.lazy <| \() -> {
          transform = \url cache ->
            let transformed =
                  functionResult |> Result.map (\function ->
                    storedField |> function
                  ) |> Remote url
                functionResult =
                  functionMappingFunctions.transform url cache |> .data
                storedField =
                  fieldMappingFunctions.transform url cache
            in transformed,
          subscribe = \address url ->
            TaskUtil.parallel [
              functionMappingFunctions.subscribe address url,
              fieldMappingFunctions.subscribe address url
            ],
          unsubscribe = \url cache ->
            TaskUtil.parallel [
              functionMappingFunctions.unsubscribe url cache,
              fieldMappingFunctions.unsubscribe url cache
            ],
          handle = \address url state ->
            TaskUtil.parallel [
              functionMappingFunctions.handle address url state,
              fieldMappingFunctions.handle address url state
            ],
          set = \model url ->
            TaskUtil.parallel [
              functionMappingFunctions.set model url,
              fieldMappingFunctions.set model url
            ]
        }
      functionMappingFunctions =
        functionMapping |> getFunctions
      fieldMappingFunctions =
        field |> fieldToMapping |> getFunctions
  in result

fieldToMapping : Field field object -> InternalMapping field object
fieldToMapping field =
  let result =
        Recursive <| Lazy.lazy <| \() -> {
          transform = \url cache ->
            mappingFunctions.transform (url +/ field.key) cache,
          subscribe = \address url ->
            mappingFunctions.subscribe address (url +/ field.key),
          unsubscribe = \url cache ->
            mappingFunctions.unsubscribe (url +/ field.key) cache,
          handle = \address url state ->
            mappingFunctions.handle address (url +/ field.key) state,
          set = \model url ->
            model
            |> field.get
            |> .data
            |> Result.toMaybe
            |> Maybe.map (\fieldModel ->
              mappingFunctions.set fieldModel (url +/ field.key)
            )
            |> TaskUtil.orDoNothing
        }
      mappingFunctions =
        field.mapping |> getFunctions
  in result

choice : Mapping choice
choice =
  let result =
        Direct {
          transform = \url cache ->
            DecodingError "Choice has no options" |> Err |> Remote url,
          subscribe = \address url ->
            subscribe address (typeUrl url),
          unsubscribe = \url cache ->
            unsubscribe (typeUrl url) cache,
          handle = \address url state ->
            Task.succeed (),
          set = \model url ->
            Task.succeed ()
        }
  in result

getTypeName : String -> Cache -> Result Error String
getTypeName url cache =
  decode Decode.string (typeUrl url) cache |> .data

typeUrl : String -> String
typeUrl url =
  url +/ "type"

valueUrl : String -> String
valueUrl url =
  url +/ "value"

type alias Option option choice =
  {
    typeName: String,
    constructor: option -> choice,
    selector: choice -> Maybe option,
    mapping: Mapping option
  }

withOption : Mapping choice -> Option option choice -> Mapping choice
withOption mapping option =
  let result =
        Recursive <| Lazy.lazy <| \() -> {
          transform = \url cache ->
            let transformed =
                  case transformedSoFar |> .data of
                    Err _ ->
                      optionMappingFunctions.transform url cache
                    Ok _ ->
                      transformedSoFar
                transformedSoFar =
                  mappingFunctions.transform url cache
            in transformed,
          subscribe = \address url ->
            mappingFunctions.subscribe address url,
          unsubscribe = \url cache ->
            TaskUtil.parallel [
              mappingFunctions.unsubscribe url cache,
              optionMappingFunctions.unsubscribe url cache
            ],
          handle = \address url state ->
            TaskUtil.parallel [
              mappingFunctions.handle address url state,
              optionMappingFunctions.handle address url state
            ],
          set = \model url ->
            TaskUtil.parallel [
              mappingFunctions.set model url,
              optionMappingFunctions.set model url
            ]
        }
      mappingFunctions =
        mapping |> getFunctions
      optionMappingFunctions =
        option |> optionToMapping |> getFunctions
  in result

optionToMapping : Option option choice -> Mapping choice
optionToMapping option =
  let result =
        Recursive <| Lazy.lazy <| \() -> {
          transform = \url cache ->
            ifSelected url cache (Ok ())
            `Result.andThen` (\_ ->
              mappingFunctions.transform (valueUrl url) cache
              |> .data
              |> Result.map option.constructor
            )
            |> Remote url,
          subscribe = \address url ->
            Task.succeed (),
          unsubscribe = \url cache ->
            mappingFunctions.unsubscribe (valueUrl url) cache
            |> doIfSelected url cache,
          handle = \address url state ->
            let result =
                  TaskUtil.parallel [
                    handleCurrentMapping,
                    unsubscribeWithOldMapping,
                    subscribeWithNewMapping
                  ]
                handleCurrentMapping =
                  mappingFunctions.handle address (valueUrl url) state
                  |> doIfSelected url state.cache
                unsubscribeWithOldMapping =
                  if thisValueChanged then
                    mappingFunctions.unsubscribe (valueUrl url) state.previousCache
                    |> doIfSelected url state.previousCache
                  else
                    Task.succeed ()
                subscribeWithNewMapping =
                  if thisValueChanged then
                    mappingFunctions.subscribe address (valueUrl url)
                    |> doIfSelected url state.cache
                  else
                    Task.succeed ()
                thisValueChanged =
                  state.lastEvent
                  |> Maybe.map (\remoteEntryEvent ->
                    remoteEntryEvent |> valueChangedAt (typeUrl url)
                  )
                  |> Maybe.withDefault False
            in result,
          set = \model url ->
            model
            |> option.selector
            |> Maybe.map (\optionModel ->
              TaskUtil.parallel [
                ElmFire.set (option.typeName |> Encode.string) (typeUrl url |> ElmFire.fromUrl)
                  |> Task.map (always ()),
                mappingFunctions.set optionModel (valueUrl url)
              ]
            )
            |> TaskUtil.orDoNothing
        }
      ifSelected : String -> Cache -> Result Error a -> Result Error a
      ifSelected url cache valueResult =
        (
          getTypeName url cache
          |> Result.formatError (\error ->
            DecodingError ("Failed to get type name: " ++ (error |> toString))
          )
        )
        `Result.andThen` (\typeName ->
          if option.typeName == typeName then
            valueResult
          else
            Err <| DecodingError ("Unsupported type name: " ++ typeName)
        )
      doIfSelected url cache task =
        Ok task
        |> ifSelected url cache
        |> Result.toMaybe
        |> TaskUtil.orDoNothing
      mappingFunctions =
        option.mapping |> getFunctions
  in result

valueChangedAt : String -> Remote EntryEvent -> Bool
valueChangedAt url remoteEntryEvent =
  if remoteEntryEvent.url == url then
    case remoteEntryEvent.data of
      ValueChanged _ ->
        True
      _ ->
        False
  else
    False

recursive : (() -> Mapping a) -> Mapping a
recursive getMapping =
  Recursive <| Lazy.lazy (\() ->
    getMapping () |> getFunctions
  )
{-
type alias Reference a =
  {
    get: () -> Stored a
  }

reference : Mapping a -> Mapping (Reference a)
reference mapping =
  let result =
        Direct {
          transform = \url cache ->
            let result =
                  getReferenceUrl url cache
                  |> Result.map (\referenceUrl ->
                    {
                      get = \() ->
                        mappingFunctions.transform referenceUrl cache
                    }
                  ) |> Remote url
            in result,
          subscribe =
            subscribe,
          unsubscribe = \url cache ->
            let result =
                  TaskUtil.parallel [
                    unsubscribe url cache,
                    getReferenceUrl url cache
                    |> Result.toMaybe
                    |> Maybe.map (\referenceUrl ->
                      mappingFunctions.unsubscribe referenceUrl cache
                    ) |> TaskUtil.orDoNothing
                  ]
            in result,
          handle = \address url state ->
            let result =
                  TaskUtil.parallel (handleMapping :: resubscribe)
                resubscribe =
                  state.lastEvent
                  |> Maybe.map (\remoteEntryEvent ->
                    if remoteEntryEvent |> valueChangedAt url then
                      [
                        unsubscribeOldUrl,
                        subscribeNewUrl
                      ]
                    else
                      []
                  )
                  |> Maybe.withDefault []
                unsubscribeOldUrl =
                  maybeOldUrl |> Maybe.map (\oldUrl ->
                    mappingFunctions.unsubscribe oldUrl state.previousCache
                  ) |> TaskUtil.orDoNothing
                subscribeNewUrl =
                  maybeNewUrl |> Maybe.map (\newUrl ->
                    mappingFunctions.subscribe address newUrl
                  ) |> TaskUtil.orDoNothing
                maybeOldUrl =
                  getReferenceUrl url state.previousCache |> Result.toMaybe
                maybeNewUrl =
                  getReferenceUrl url state.cache |> Result.toMaybe
                handleMapping =
                  maybeNewUrl |> Maybe.map (\newUrl ->
                    mappingFunctions.handle address newUrl state
                  ) |> TaskUtil.orDoNothing
            in result
        }
      mappingFunctions =
        mapping |> getFunctions
      getReferenceUrl url cache =
        decode Decode.string url cache
        |> .data
  in result

type alias Many a =
  List (Stored a)

many : Mapping a -> Mapping (Many a)
many mapping =
  let result =
        Direct {
          transform = \url cache ->
            let result =
                  getChildren url cache
                  |> Result.map (\keys ->
                    keys
                    |> List.map (\key ->
                      mappingFunctions.transform (url +/ key) cache
                    )
                  )
                  |> Remote url
            in result,
          subscribe =
            subscribe,
          unsubscribe =
            unsubscribe,
          handle = \address url state ->
            let result =
                  TaskUtil.parallel (resubscribe ++ handleChildren)
                resubscribe =
                  state.lastEvent
                  |> Maybe.map (\remoteEntryEvent ->
                    if remoteEntryEvent |> valueChangedAt url then
                      unsubscribeRemovedChildren ++ subscribeAddedChildren
                    else
                      []
                  )
                  |> Maybe.withDefault []
                unsubscribeRemovedChildren =
                  removedChildren |> List.map unsubscribeOldChild
                subscribeAddedChildren =
                  addedChildren |> List.map subscribeNewChild
                removedChildren =
                  Set.diff previousChildren children |> Set.toList
                addedChildren =
                  Set.diff children previousChildren |> Set.toList
                previousChildren =
                  getChildrenList state.previousCache |> Set.fromList
                children =
                  getChildrenList state.cache |> Set.fromList
                getChildrenList cache =
                  getChildren url cache
                  |> Result.toMaybe
                  |> Maybe.withDefault []
                handleChildren =
                  getChildrenList state.cache
                  |> List.map (\key ->
                    mappingFunctions.handle address (url +/ key) state
                  )
                unsubscribeOldChild key =
                  mappingFunctions.unsubscribe (url +/ key) state.previousCache
                subscribeNewChild key =
                  mappingFunctions.subscribe address (url +/ key)
            in result
        }
      mappingFunctions =
        mapping |> getFunctions
      getChildren url cache =
        decode (Decode.dict (Decode.succeed ()) |> Decode.map Dict.keys) url cache
        |> .data
  in result
-}
(+/) : String -> String -> String
(+/) base suffix =
  base ++ "/" ++ suffix
