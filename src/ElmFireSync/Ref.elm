module ElmFireSync.Ref where

import Signal exposing (Address)
import Json.Decode as Decode exposing (Value, Decoder)
import Task exposing (Task)
import Effects exposing (Never)
import Debug
import ElmFire exposing (Location, Subscription, Reference, Snapshot, Cancellation(..), ErrorType(..), Priority)
import TaskUtil
import Component exposing (Update)
import ElmFireSync.Handler exposing (Handler)

type alias Ref a =
  {
    url: String,
    handler: Handler a,
    state: Result NoSubscription (State a)
  }

type NoSubscription =
  NotSubscribed |
  SubscriptionFailed ElmFire.Error

type alias State a =
  {
    subscription: Subscription,
    data: Result NoData a
  }

type NoData =
  Loading |
  DecodingFailed String

type Error =
  NoSubscription NoSubscription |
  NoData NoData

type Action a =
  None |
  SubscriptionError ElmFire.Error |
  Subscribed Subscription |
  ValueChanged Snapshot |
  Unsubscribe

init : Address (Action a) -> Handler a -> String -> Update (Ref a) (Action a)
init address handler url =
  {
    model =
      {
        url =
          url,
        handler =
          handler,
        state =
          Err NotSubscribed
      },
    effects =
      url
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
update : (Action a) -> (Ref a) -> Update (Ref a) (Action a)
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
            model.state |> Result.toMaybe
            |> Maybe.map (always model) -- we are already subscribed
            |> Maybe.withDefault { model | state <- Ok updatedState } -- not yet
          updatedState =
            {
              subscription =
                subscription,
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
              data =
                snapshot.value
                |> Decode.decodeValue model.handler.decoder
                |> Result.formatError DecodingFailed
            }
      in result
    Unsubscribe ->
      {
        model =
          { model | state <- Err NotSubscribed },
        effects =
          model.state |> Result.toMaybe
          |> Maybe.map (\state ->
            state.subscription |> ElmFire.unsubscribe
            |> TaskUtil.swallowError None "ElmFire.unsubscribe failed"
            |> Effects.task
          )
          |> Maybe.withDefault Effects.none
      }

get : Ref a -> Result Error a
get model =
  (model.state |> Result.formatError NoSubscription)
  `Result.andThen` (\state ->
    state.data |> Result.formatError NoData
  )

set : Maybe Priority -> a -> Ref a -> Task ElmFire.Error Reference
set maybePriority value model =
  let result =
        maybePriority
        |> Maybe.map (\priority ->
          ElmFire.setWithPriority json priority location
        )
        |> Maybe.withDefault (ElmFire.set json location)
      json =
        value |> model.handler.encode
      location =
        model.url |> ElmFire.fromUrl
  in result

delete : Ref a -> Task ElmFire.Error Reference
delete model =
  ElmFire.remove (model.url |> ElmFire.fromUrl)
