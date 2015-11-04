module ElmFireSync.Ref where

import Signal exposing (Address)
import Json.Decode as Decode exposing (Value, Decoder)
import Task exposing (Task)
import Effects exposing (Never)
import Debug
import ElmFire exposing (Location, Subscription, Reference, Snapshot, Cancellation(..), ErrorType(..), Priority(..))
import TaskUtil
import Component exposing (Update)
import ElmFireSync.Codec exposing (Codec)

type alias Ref a =
  {
    context: Context a,
    state: Result NoSubscription (State a)
  }

type alias Context a =
  {
    url: String,
    codec: Codec a
  }

type NoSubscription =
  NotSubscribed |
  SubscriptionFailed ElmFire.Error

type alias State a =
  {
    subscription: Subscription,
    priority: Priority,
    data: Result NoData a
  }

type NoData =
  Loading |
  DecodingFailed String

type Action a =
  None |
  SubscriptionError ElmFire.Error |
  Subscribed Subscription |
  ValueChanged Snapshot

init : Context a -> Address (Action a) -> Update (Ref a) (Action a)
init context address =
  {
    model =
      {
        context =
          context,
        state =
          Err NotSubscribed
      },
    effects =
      context.url
      |> ElmFire.fromUrl
      |> ElmFire.subscribe
        (\snapshot ->
          snapshot |> ValueChanged |> Signal.send address
        )
        (\cancellation ->
          let result =
                error |> SubscriptionError |> Signal.send address
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
        (ElmFire.valueChanged <| ElmFire.noOrder)
      |> Task.map Subscribed
      |> TaskUtil.onError (\error ->
        SubscriptionError error |> Task.succeed
      )
      |> Effects.task
  }

{-- Do not call this with a concrete action, use only for propagation!
-}
update : Action a -> Ref a -> Update (Ref a) (Action a)
update action model =
  case action of
    None ->
      Component.return model
    SubscriptionError error ->
      Component.return
        { model | state <- Err <| SubscriptionFailed error }
    Subscribed subscription ->
      let result =
            Component.return updatedModel
          updatedModel =
            model.state
            |> Result.toMaybe
            |> Maybe.map (always model) -- we are already subscribed
            |> Maybe.withDefault { model | state <- Ok updatedState } -- not yet
          updatedState =
            {
              subscription =
                subscription,
              priority =
                NoPriority,
              data =
                Err Loading
            }
      in result
    ValueChanged snapshot ->
      let result =
            Component.return
              { model | state <- Ok updatedState }
          updatedState =
            {
              subscription =
                snapshot.subscription,
              priority =
                snapshot.priority,
              data =
                snapshot.value
                |> Decode.decodeValue model.context.codec.decoder
                |> Result.formatError DecodingFailed
            }
      in result

type Error =
  NoSubscription NoSubscription |
  NoData NoData

get : Ref a -> Result Error a
get model =
  (model.state |> Result.formatError NoSubscription)
  `Result.andThen` (\state ->
    state.data |> Result.formatError NoData
  )

getPriority : Ref a -> Priority
getPriority model =
  model.state
  |> Result.toMaybe
  |> Maybe.map .priority
  |> Maybe.withDefault NoPriority

set : a -> Ref a -> Task ElmFire.Error Reference
set value model =
  let result =
        ElmFire.setWithPriority json priority location
      json =
        value |> model.context.codec.encode
      priority =
        model |> getPriority
      location =
        model.context.url |> ElmFire.fromUrl
  in result

delete : Ref a -> Task ElmFire.Error Reference
delete model =
  model.context.url |> ElmFire.fromUrl |> ElmFire.remove

{-- Do not call!
-}
unsubscribe : Ref a -> Task ElmFire.Error ()
unsubscribe model =
  model.state
  |> Result.toMaybe
  |> Maybe.map (\state ->
    state.subscription |> ElmFire.unsubscribe
  )
  |> Maybe.withDefault (Task.succeed ())
