module StructuralEditor.Editor where

import Html exposing (Html)
import Debug
import Dict exposing (Dict)
import Signal exposing (Address)
import ElmFire exposing (Location, Subscription, Snapshot, Cancellation(..), ErrorType(..), Priority(..))
import TaskUtil
import Component exposing (Update)

type alias Editor model =
  {
    location: Location,
    subscriptions: Dict EventType (Result ElmFire.Error Subscription),
    priority: Priority,
    model: model
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

type Action action =
  SubscriptionResult EventType (Result ElmFire.Error Subscription) |
  Event EventType Snapshot |
  CustomAction action

init : model -> Location -> Address (Action action) -> Update (Editor model)
init initialModel location address =
  let result =
        Component.returnAndRun model task
      model =
        {
          location =
            location,
          subscriptions =
            Dict.empty,
          priority =
            NoPriority,
          model =
            initialModel
        }
      task =
        [
          subscribe valueChanged ElmFire.valueChanged,
          subscribe childAdded ElmFire.childAdded,
          subscribe childRemoved ElmFire.childRemoved,
          subscribe childMoved ElmFire.childMoved
        ]
        |> TaskUtil.parallel
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

done : Editor model -> Component.HandledTask
done editor =
  editor.subscriptions
  |> Dict.values
  |> List.map Result.toMaybe
  |> List.filterMap identity
  |> List.map (\subscription ->
    subscription |> ElmFire.unsubscribe |> TaskUtil.swallowError "ElmFire.unsubscribe failed"
  )
  |> TaskUtil.parallel

type alias UpdateContext editor action = -- TODO
  {
    valueChanged: EventHandler editor action,
    childAdded: EventHandler editor action,
    childRemoved: EventHandler editor action,
    childMoved: EventHandler editor action,
    customAction: Address action -> action -> editor -> Update editor
  }

type alias EventHandler editor action =
  Address action -> Snapshot -> editor -> Update editor

defaultUpdateContext : UpdateContext editor action
defaultUpdateContext =
  {
    valueChanged _ _ editor =
      Component.return editor,
    childAdded _ _ editor =
      Component.return editor,
    childRemoved _ _ editor =
      Component.return editor,
    childMoved _ _ editor =
      Component.return editor,
    customAction _ _ editor =
      Component.return editor
  }

{-- Do not call this with a concrete action, use only for propagation!
-}
update : UpdateContext (Editor model) action -> Address (Action action) -> Action action -> Editor model -> Update (Editor model)
update context address action editor =
  case action of
    SubscriptionResult eventType subscriptionResult ->
      Component.return
        { editor |
          subscriptions <-
            editor.subscriptions |> Dict.insert eventType subscriptionResult
        }
    Event eventType snapshot ->
      let result =
            Component.returnAndRun
              { updatedEditor | priority <- snapshot.priority }
              update.task
          updatedEditor =
            update.model
          update =
            handler
              (address `Signal.forwardTo` CustomAction)
              snapshot
              editor
          handler =
            if eventType == valueChanged then
                context.valueChanged
            else if eventType == childAdded then
                context.childAdded
            else if eventType == childRemoved then
                context.childRemoved
            else if eventType == childMoved then
                context.childMoved
            else
                dummyHandler |> Debug.log "unhandled event type"
          dummyHandler _ _ _ =
            Component.return editor
      in result
    CustomAction action ->
      context.customAction
        (address `Signal.forwardTo` CustomAction)
        action
        editor

type alias ViewContext model action =
  {
    view: {-focused:-} Bool -> Address action -> Editor model -> Html
  }

view : ViewContext model action -> Bool -> Address (Action action) -> Editor model -> Html
view context focused address editor =
  context.view
    focused
    (address `Signal.forwardTo` CustomAction)
    editor
