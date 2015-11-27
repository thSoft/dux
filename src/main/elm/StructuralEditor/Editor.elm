module StructuralEditor.Editor where

import Html exposing (Html)
import Html.Attributes as Attributes
import Debug
import Dict exposing (Dict)
import Signal exposing (Address)
import Task exposing (Task)
import Task.Extra
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

type alias UpdateContext model action =
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
update : UpdateContext model action -> Address (Action action) -> Action action -> Editor model -> Update (Editor model)
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
              { editor |
                priority <- snapshot.priority,
                model <- update.model
              }
              update.task
          update =
            handler
              (address `Signal.forwardTo` CustomAction)
              snapshot
              model
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
            Component.return model
          model =
            editor.model
      in result
    CustomAction action ->
      let result =
            Component.returnAndRun
              { editor | model <- update.model }
              update.task
          update =
            context.customAction
              (address `Signal.forwardTo` CustomAction)
              action
              editor.model
      in result

type alias ViewContext model action =
  {
    view: {-focused:-} Bool -> Address action -> Editor model -> Html
  }

view : Bool -> ViewContext model action -> Address (Action action) -> Editor model -> Html
view focused context address editor =
  context.view
    focused
    (address `Signal.forwardTo` CustomAction)
    editor

focusAttributes : Bool -> List Html.Attribute
focusAttributes focused =
  if focused then
    [Attributes.attribute "data-autofocus" "true"]
  else
    []
