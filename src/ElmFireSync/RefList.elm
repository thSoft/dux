module ElmFireSync.RefList where

import Dict exposing (Dict)
import Signal exposing (Address, Mailbox)
import Task exposing (Task)
import Effects
import Debug
import ElmFire exposing (Location, Priority, Snapshot)
import TaskUtil
import Component exposing (Update)
import ElmFireSync.Ref as Ref

type alias Model a =
  {
    location: Location,
    childHandler: Ref.Handler a,
    children: Dict String (Child a)
  }

type alias Child a =
  {
    priority: Priority,
    ref: Ref.Model a
  }

type Action a =
  None |
  ChildAdded Snapshot |
  ChildRemoved Snapshot |
  RefAction String (Ref.Action a)

init : Address (Action a) -> Ref.Handler a -> Location -> Update (Model a) (Action a)
init address childHandler location =
  let result =
        {
          model =
            {
              location =
                location,
              childHandler =
                childHandler,
              children =
                Dict.empty
            },
          effects =
            Effects.batch [
              subscribe ChildAdded ElmFire.childAdded,
              subscribe ChildRemoved ElmFire.childRemoved
            ]
        }
      subscribe action query =
        ElmFire.subscribe
          (\snapshot ->
            snapshot |> action |> Signal.send address
          )
          (\cancellation ->
            cancellation |> Debug.log "ElmFire.subscribe cancelled"
            |> Task.succeed
            |> Task.map (always ())
          )
          (query <| ElmFire.orderByPriority ElmFire.noRange ElmFire.noLimit)
          location
        |> TaskUtil.toEffects None "ElmFire.subscribe failed"
  in result

update : Address (Action a) -> Action a -> Model a -> Update (Model a) (Action a)
update address action model =
  case action of
    None ->
      Component.return model
    ChildAdded snapshot ->
      let result =
            {
              model =
                { model | children <- model.children |> Dict.insert url child },
              effects =
                childUpdate.effects |> Effects.map (RefAction url)
            }
          url =
            snapshot.reference |> ElmFire.toUrl
          child =
            {
              priority =
                snapshot.priority,
              ref =
                childUpdate.model
            }
          childUpdate =
            snapshot.reference |> ElmFire.location |> Ref.init childAddress model.childHandler
          childAddress =
            address `Signal.forwardTo` (RefAction url)
      in result
    ChildRemoved snapshot ->
      delegateToChild model (always Nothing) (snapshot.reference |> ElmFire.toUrl) Ref.Unsubscribe
    RefAction url refAction ->
      delegateToChild model Just url refAction

delegateToChild : Model a -> (Child a -> Maybe (Child a)) -> String -> Ref.Action a -> Update (Model a) (Action a)
delegateToChild model transformUpdatedChild url refAction =
  model.children |> Dict.get url |> Maybe.map (\child ->
    let result =
          {
            model =
              { model |
                children <- model.children |> Dict.update url (always (updatedChild |> transformUpdatedChild)) },
            effects =
              childUpdate.effects |> Effects.map (RefAction url)
          }
        updatedChild =
          { child | ref <- childUpdate.model }
        childUpdate =
          child.ref |> Ref.update refAction
    in result
  ) |> Maybe.withDefault (Component.return model)

get : Model a -> List (Ref.Model a)
get model =
  model.children |> Dict.values |> List.map .ref
