module ElmFireSync.Ref where

import Debug
import Dict exposing (Dict)
import Signal exposing (Address)
import Task exposing (Task)
import Task.Extra
import ElmFire exposing (Location, Subscription, Snapshot, Cancellation(..), ErrorType(..), Priority(..))
import TaskUtil
import Component exposing (Update)

type alias Ref model =
  {
    location: Location,
    subscriptions: Dict EventType (Result ElmFire.Error Subscription),
    state: State model
  }

type alias EventType =
  String -- XXX Elm doesn't support enums

valueChanged : EventType
valueChanged =
  "valueChanged"

childAdded : EventType
childAdded =
  "childAdded"

childRemoved : EventType
childRemoved =
  "childRemoved"

childMoved : EventType
childMoved =
  "childMoved"

type alias State model =
  {
    priority: Priority,
    model: model
  }

type Action action =
  None |
  SubscriptionResult EventType (Result ElmFire.Error Subscription) |
  Event EventType Snapshot |
  CustomAction action

init : model -> Location -> Address (Action action) -> Update (Ref model)
init initialModel location address =
  let result =
        Component.returnAndRun model task
      model =
        {
          location =
            location,
          subscriptions =
            Dict.empty,
          state =
            {
              priority =
                NoPriority,
              model =
                initialModel
            }
        }
      task =
        [
          subscribe valueChanged ElmFire.valueChanged,
          subscribe childAdded ElmFire.childAdded,
          subscribe childRemoved ElmFire.childRemoved,
          subscribe childMoved ElmFire.childMoved
        ]
        |> Task.Extra.parallel
        |> Task.map (always ())
      subscribe eventType query =
        location
        |> ElmFire.subscribe
          (\snapshot ->
            snapshot |> Event eventType |> Signal.send address
          )
          (\cancellation ->
            let result =
                  error |> Err |> SubscriptionResult eventType |> Signal.send address
                error =
                  case cancellation of
                    Unsubscribed _ ->
                      cancellationError
                    QueryError _ queryError ->
                      queryError
                cancellationError =
                  {
                    tag =
                      OtherFirebaseError,
                    description =
                      "Subscription was cancelled"
                  }
            in result
          )
          (query <| ElmFire.noOrder)
        |> TaskUtil.andThen (TaskUtil.notify address (Ok >> (SubscriptionResult eventType)))
        |> TaskUtil.onError (TaskUtil.notify address (Err >> (SubscriptionResult eventType)))
  in result

type alias Kind model action =
  {
    valueChanged: EventHandler model action,
    childAdded: EventHandler model action,
    childRemoved: EventHandler model action,
    childMoved: EventHandler model action,
    customAction: Address action -> action -> model -> Update model
  }

type alias EventHandler model action =
  Address action -> Snapshot -> model -> Update model

{-- Do not call this with a concrete action, use only for propagation!
-}
update : Kind model action -> Address (Action action) -> Action action -> Ref model -> Update (Ref model)
update kind address action ref =
  case action of
    None ->
      Component.return ref
    SubscriptionResult eventType subscriptionResult ->
      Component.return
        { ref |
          subscriptions <-
            ref.subscriptions |> Dict.insert eventType subscriptionResult
        }
    Event eventType snapshot ->
      let result =
            Component.returnAndRun
              { ref | state <- updatedState }
              update.task
          updatedState =
            {
              priority =
                snapshot.priority,
              model =
                update.model
            }
          update =
            handler
              (address `Signal.forwardTo` CustomAction)
              snapshot
              model
          handler =
            if eventType == valueChanged then
                kind.valueChanged
            else if eventType == childAdded then
                kind.childAdded
            else if eventType == childRemoved then
                kind.childRemoved
            else if eventType == childMoved then
                kind.childMoved
            else
                dummyHandler |> Debug.log "unhandled event type"
          dummyHandler _ _ _ =
            Component.return model
          model =
            ref |> getModel
      in result
    CustomAction action ->
      let result =
            Component.returnAndRun
              { ref | state <- updatedState }
              update.task
          updatedState =
            { oldState | model <- update.model }
          oldState =
            ref.state
          update =
            kind.customAction
              (address `Signal.forwardTo` CustomAction)
              action
              (ref |> getModel)
      in result

getModel : Ref model -> model
getModel ref =
  ref.state.model

getPriority : Ref model -> Priority
getPriority ref =
  ref.state.priority

{-- Do not call!
-}
unsubscribe : Ref model -> Task ElmFire.Error ()
unsubscribe ref =
  ref.subscriptions
  |> Dict.values
  |> List.map (\result ->
    result
    |> Result.toMaybe
    |> Maybe.map ElmFire.unsubscribe
    |> Maybe.withDefault (Task.succeed ())
  )
  |> Task.Extra.parallel
  |> Task.map (always ())
