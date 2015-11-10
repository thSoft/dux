module ElmFireSync.ListRef where

import Dict exposing (Dict)
import Signal exposing (Address, Mailbox)
import Task exposing (Task)
import Debug
import Task.Extra
import ElmFire exposing (Location, Priority(..), Snapshot)
import TaskUtil
import Component exposing (Update)
import ElmFireSync.ItemHandler exposing (ItemHandler)

type alias ListRef model action =
  {
    location: Location,
    itemHandler: ItemHandler model action,
    items: Dict String (Item model)
  }

type alias Item model =
  {
    priority: Priority,
    data: model
  }

type Action action =
  None |
  ChildAdded Snapshot |
  ChildRemoved Snapshot |
  ChildMoved Snapshot |
  ItemAction String action

init : ItemHandler model action -> Location -> Address (Action action) -> Update (ListRef model action)
init itemHandler location address =
  let result =
        Component.returnAndRun
          {
            itemHandler =
              itemHandler,
            location =
              location,
            items =
              Dict.empty
          }
          (
            Task.Extra.parallel [
              subscribe ChildAdded ElmFire.childAdded,
              subscribe ChildRemoved ElmFire.childRemoved,
              subscribe ChildMoved ElmFire.childMoved
            ]
            |> Task.map (always ())
          )
      subscribe action query =
        ElmFire.subscribe
          (\snapshot ->
            snapshot |> action |> Signal.send address
          )
          (\cancellation ->
            cancellation
            |> Debug.log "Subscription cancelled"
            |> Task.succeed
            |> Task.map (always ())
          )
          (query <| ElmFire.noOrder)
          location
        |> TaskUtil.swallowError "Subscription failed"
  in result

update : Address (Action action) -> Action action -> ListRef model action -> Update (ListRef model action)
update address action model =
  case action of
    None ->
      Component.return model
    ChildAdded snapshot ->
      let result =
            Component.returnAndRun
              { model | items <- model.items |> Dict.insert url item }
              itemUpdate.task
          url =
            snapshot.reference |> ElmFire.toUrl
          item =
            {
              priority =
                snapshot.priority,
              data =
                itemUpdate.model
            }
          itemUpdate =
            model.itemHandler.init itemAddress url
          itemAddress =
            address `Signal.forwardTo` (ItemAction url)
      in result
    ChildRemoved snapshot ->
      let result =
            model.items |> Dict.get url |> Maybe.map (\item ->
              Component.returnAndRun
                { model | items <- model.items |> Dict.remove url }
                (
                  item.data
                  |> model.itemHandler.done itemAddress
                )
            ) |> Maybe.withDefault (Component.return model)
          url =
            snapshot.reference |> ElmFire.toUrl
          itemAddress =
            address `Signal.forwardTo` (ItemAction url)
      in result
    ChildMoved snapshot ->
      let result =
            Component.return
              { model |
                items <-
                  model.items |> Dict.update url (Maybe.map (\item ->
                    { item | priority <- snapshot.priority }
                  ))
              }
          url =
            snapshot.reference |> ElmFire.toUrl
      in result
    ItemAction url itemAction ->
      model.items |> Dict.get url |> Maybe.map (\item ->
        let result =
              Component.returnAndRun
                { model | items <- model.items |> Dict.update url (always updatedItem) }
                itemUpdate.task
            updatedItem =
              Just { item | data <- itemUpdate.model }
            itemUpdate =
              model.itemHandler.update itemAddress itemAction item.data
            itemAddress =
              address `Signal.forwardTo` (ItemAction url)
        in result
      ) |> Maybe.withDefault (Component.return model)

get : ListRef model action -> List (String, Item model)
get model =
  model.items |> Dict.toList |> List.sortBy internalPriority

internalPriority : (String, Item model) -> (Int, Float, String, String)
internalPriority (url, item) =
  case item.priority of
    NoPriority ->
      (0, 0, "", url)
    NumberPriority priority ->
      (1, priority, "", url)
    StringPriority priority ->
      (2, 0, priority, url)

-- TODO handle concurrency
fixPriorities : ListRef model action -> Task ElmFire.Error ()
fixPriorities model =
  model
  |> get
  |> List.indexedMap (\index (url, item) ->
    ElmFire.setPriority (index |> toFloat |> NumberPriority) (url |> ElmFire.fromUrl)
  )
  |> Task.Extra.parallel
  |> Task.map (always ())
