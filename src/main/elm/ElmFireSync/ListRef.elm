module ElmFireSync.ListRef where

import Dict exposing (Dict)
import Signal exposing (Address)
import ElmFire exposing (Priority(..), Location)
import Component exposing (Update)
import ElmFireSync.ItemHandler exposing (ItemHandler)
import ElmFireSync.Ref as Ref exposing (Ref)

type alias ListRef data =
  Ref (Model data)

type alias Model data =
  Dict String (Item data)

type alias Item data =
  {
    priority: Priority,
    data: data
  }

type alias Action action =
  {
    url: String,
    action: action
  }

getAction : String -> action -> Action action
getAction url action =
  {
    url =
      url,
    action =
      action
  }

init : Location -> Address (Ref.Action action) -> Update (ListRef model)
init =
  Ref.init initialModel

initialModel : Model data
initialModel =
  Dict.empty

update : ItemHandler data action -> Address (Ref.Action (Action action)) -> Ref.Action (Action action) -> ListRef data -> Update (ListRef data)
update itemHandler =
  Ref.update (kind itemHandler)

kind : ItemHandler data action -> Ref.Kind (Model data) (Action action)
kind itemHandler =
  {
    valueChanged _ _ model =
      Component.return model,
    childAdded address snapshot model =
      let result =
            Component.returnAndRun
              (model |> Dict.insert url item)
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
            itemHandler.init itemAddress url
          itemAddress =
            address `Signal.forwardTo` (getAction url)
      in result,
    childRemoved address snapshot model =
      let result =
            model |> Dict.get url |> Maybe.map (\item ->
              Component.returnAndRun
                (model |> Dict.remove url)
                (
                  item.data
                  |> itemHandler.done itemAddress
                )
            ) |> Maybe.withDefault (Component.return model)
          url =
            snapshot.reference |> ElmFire.toUrl
          itemAddress =
            address `Signal.forwardTo` (Action url)
      in result,
    childMoved _ snapshot model =
      let result =
            Component.return
              (model |> Dict.update url (Maybe.map (\item ->
                { item | priority <- snapshot.priority }
              )))
          url =
            snapshot.reference |> ElmFire.toUrl
      in result,
    customAction address action model =
      model |> Dict.get action.url |> Maybe.map (\item ->
        let result =
              Component.returnAndRun
                (model |> Dict.insert action.url updatedItem)
                itemUpdate.task
            updatedItem =
              { item | data <- itemUpdate.model }
            itemUpdate =
              itemHandler.update itemAddress action.action item.data
            itemAddress =
              address `Signal.forwardTo` (getAction action.url)
        in result
      ) |> Maybe.withDefault (Component.return model)
  }

{-- Returns the list of items along with their URLs ordered by priority.
-}
get : Ref (Model model) -> List (String, Item model)
get ref =
  ref.model
  |> Dict.toList
  |> List.sortBy internalPriority

internalPriority : (String, Item model) -> (Int, Float, String, String)
internalPriority (url, item) =
  case item.priority of
    NoPriority ->
      (0, 0, "", url)
    NumberPriority priority ->
      (1, priority, "", url)
    StringPriority priority ->
      (2, 0, priority, url)
