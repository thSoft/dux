module ElmFireSync.Ref where

import Signal exposing (Address)
import Json.Decode as Decode exposing (Value, Decoder)
import Task exposing (Task)
import Effects exposing (Never)
import Debug
import ElmFire exposing (Location, Subscription, Snapshot, Cancellation(..), ErrorType(..))
import TaskUtil
import Component exposing (Update)

type alias Model a =
  {
    location: Location,
    handler: Handler a,
    state: Result NoSubscription (State a)
  }

type alias Handler a =
  {
    decoder: Decoder a,
    encode: a -> Value
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

get : Model a -> Result Error a
get model =
  (model.state |> Result.formatError NoSubscription)
  `Result.andThen` (\state ->
    state.data |> Result.formatError NoData
  )

type Error =
  NoSubscription NoSubscription |
  NoData NoData

init : Address (Action a) -> Handler a -> Location -> Update (Model a) (Action a)
init address handler location =
  {
    model =
      {
        location =
          location,
        handler =
          handler,
        state =
          Err NotSubscribed
      },
    effects =
      location
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

type Action a =
  None |
  SubscriptionError ElmFire.Error |
  Subscribed Subscription |
  ValueChanged Snapshot |
  Unsubscribe

{-- Do not call this with a concrete action, use only for propagation!
-}
update : (Action a) -> (Model a) -> Update (Model a) (Action a)
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
            |> TaskUtil.toEffects None "ElmFire.unsubscribe failed"
          )
          |> Maybe.withDefault Effects.none
      }

set : a -> Model a -> Task ElmFire.Error (Model a)
set value model =
  let result =
        ElmFire.set json model.location
        |> Task.map (always model)
      json =
        value |> model.handler.encode
  in result

delete : Model a -> Task ElmFire.Error (Model a)
delete model =
  ElmFire.remove model.location
  |> Task.map (always model)
