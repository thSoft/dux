module ElmFireSync.ListRef where

import Dict exposing (Dict)
import Signal exposing (Address, Mailbox)
import Task exposing (Task)
import Effects exposing (Effects)
import Debug
import Task.Extra
import ElmFire exposing (Priority(..), Snapshot)
import TaskUtil
import Component exposing (Update)
import ElmFireSync.ItemHandler exposing (ItemHandler)
import ElmFireSync.Ref as Ref exposing (Ref)

type alias ListRef model action =
  {
    context: Context model action,
    items: Dict String (Item model)
  }

type alias Context model action =
  {
    url: String,
    itemHandler: ItemHandler model action
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

init : Context model action -> Address (Action action) -> Update (ListRef model action) (Action action)
init context address =
  let result =
        {
          model =
            {
              context =
                context,
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
            cancellation |> Debug.log "Subscription cancelled"
            |> Task.succeed
            |> Task.map (always ())
          )
          (query <| ElmFire.noOrder)
          (context.url |> ElmFire.fromUrl)
        |> TaskUtil.swallowError None "Subscription failed"
        |> Effects.task
  in result

update : Address (Action action) -> Action action -> ListRef model action -> Update (ListRef model action) (Action action)
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
                itemUpdate.effects |> Effects.map (ItemAction url)
            }
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
            url |> model.context.itemHandler.init
          itemAddress =
            address `Signal.forwardTo` (ItemAction url)
      in result
    ChildRemoved snapshot ->
      let result =
            model.items |> Dict.get url |> Maybe.map (\item ->
              {
                model =
                  { model |
                    items <- model.items |> Dict.remove url },
                effects =
                  item.data
                  |> model.context.itemHandler.done
                  |> Effects.task
                  |> Effects.map (ItemAction url)
              }
            ) |> Maybe.withDefault (Component.return model)
          url =
            snapshot.reference |> ElmFire.toUrl
      in result
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
    ItemAction url itemAction ->
      model.items |> Dict.get url |> Maybe.map (\item ->
        let result =
              {
                model =
                  { model |
                    items <- model.items |> Dict.update url (always updatedItem) },
                effects =
                  itemUpdate.effects |> Effects.map (ItemAction url)
              }
            updatedItem =
              Just { item | data <- itemUpdate.model }
            itemUpdate =
              model.context.itemHandler.update itemAction item.data
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
