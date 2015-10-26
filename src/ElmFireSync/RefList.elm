module ElmFireSync.RefList where

import Dict exposing (Dict)
import Signal exposing (Address, Mailbox)
import Task exposing (Task)
import Effects exposing (Effects)
import Debug
import Task.Extra
import ElmFire exposing (Priority(..), Snapshot)
import TaskUtil
import Component exposing (Update)
import ElmFireSync.Handler exposing (Handler)
import ElmFireSync.Ref as Ref exposing (Ref)

type alias RefList a =
  {
    url: String,
    itemHandler: Handler a,
    items: Dict String (Item a)
  }

type alias Item a =
  {
    priority: Priority,
    ref: Ref a
  }

type Action a =
  None |
  ChildAdded Snapshot |
  ChildRemoved Snapshot |
  ChildMoved Snapshot |
  RefAction String (Ref.Action a)

init : Address (Action a) -> Handler a -> String -> Update (RefList a) (Action a)
init address itemHandler url =
  let result =
        {
          model =
            {
              url =
                url,
              itemHandler =
                itemHandler,
              items =
                Dict.empty
            },
          effects =
            Effects.batch [
              subscribe ChildAdded ElmFire.childAdded,
              subscribe ChildRemoved ElmFire.childRemoved,
              subscribe ChildMoved ElmFire.childMoved
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
          (query <| ElmFire.noOrder)
          (url |> ElmFire.fromUrl)
        |> TaskUtil.swallowError None "ElmFire.subscribe failed"
        |> Effects.task
  in result

update : Address (Action a) -> Action a -> RefList a -> Update (RefList a) (Action a)
update address action model =
  case action of
    None ->
      Component.return model
    ChildAdded snapshot ->
      let result =
            {
              model =
                { model | items <- model.items |> Dict.insert url item },
              effects =
                itemUpdate.effects |> Effects.map (RefAction url)
            }
          url =
            snapshot.reference |> ElmFire.toUrl
          item =
            {
              priority =
                snapshot.priority,
              ref =
                itemUpdate.model
            }
          itemUpdate =
            snapshot.reference |> ElmFire.toUrl |> Ref.init itemAddress model.itemHandler
          itemAddress =
            address `Signal.forwardTo` (RefAction url)
      in result
    ChildRemoved snapshot ->
      delegateToChild model (always Nothing) (snapshot.reference |> ElmFire.toUrl) Ref.Unsubscribe
    ChildMoved snapshot ->
      let result =
            Component.return
              { model | items <-
                  model.items |> Dict.update url (Maybe.map (\item ->
                    { item | priority <- snapshot.priority }
                  ))
              }
          url =
            snapshot.reference |> ElmFire.toUrl
      in result
    RefAction url refAction ->
      delegateToChild model (Just) url refAction

delegateToChild : RefList a -> (Item a -> Maybe (Item a)) -> String -> Ref.Action a -> Update (RefList a) (Action a)
delegateToChild model transformUpdatedChild url refAction =
  model.items |> Dict.get url |> Maybe.map (\item ->
    let result =
          {
            model =
              { model |
                items <- model.items |> Dict.update url (always (updatedChild |> transformUpdatedChild)) },
            effects =
              itemUpdate.effects |> Effects.map (RefAction url)
          }
        updatedChild =
          { item | ref <- itemUpdate.model }
        itemUpdate =
          item.ref |> Ref.update refAction
    in result
  ) |> Maybe.withDefault (Component.return model)

get : RefList a -> List (Item a)
get model =
  model.items |> Dict.values |> List.sortBy internalPriority

internalPriority : Item a -> (Int, Float, String, String)
internalPriority item =
  case item.priority of
    NoPriority ->
      (0, 0, "", item.ref.url)
    NumberPriority priority ->
      (1, priority, "", item.ref.url)
    StringPriority priority ->
      (2, 0, priority, item.ref.url)

-- TODO handle concurrency
fixPriorities : RefList a -> Task ElmFire.Error ()
fixPriorities model =
  model
  |> get
  |> List.indexedMap (\index item ->
    ElmFire.setPriority (index |> toFloat |> NumberPriority) (item.ref.url |> ElmFire.fromUrl)
  )
  |> Task.Extra.parallel
  |> Task.map (always ())
