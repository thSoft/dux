module Component where

import Signal exposing (Address)
import Html exposing (Html)
import Effects exposing (Effects)
import StartApp exposing (App)

type alias Component model action =
  {
    init: Update model action,
    update: action -> model -> Update model action,
    view: Address action -> model -> Html,
    inputs: List (Signal action)
  }

type alias Update model action =
  {
    model: model,
    effects: Effects action
  }

return : model -> Update model action
return model =
  {
    model =
      model,
    effects =
      Effects.none
  }

returnAndRun: Effects action -> model -> Update model action
returnAndRun effects model =
  {
    model =
      model,
    effects =
      effects
  }

start : Component model action -> App model
start component =
  StartApp.start
    {
      init =
        component.init |> adapt,
      update action model =
        component.update action model |> adapt,
      view =
        component.view,
      inputs =
        component.inputs
    }

adapt : Update model action -> (model, Effects action)
adapt update =
  (update.model, update.effects)
