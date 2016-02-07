module Test.Dux.Environment.Mappings where

import Task exposing (Task)
import Html exposing (Html)
import Dux.Environment.Types exposing (..)
import Dux.Environment.Mappings as Mappings
import Dux.Environment.View as View
import FirebaseModel.Mapping as Mapping

baseUrl : String
baseUrl = "https://thsoft.firebaseio.com/DUX/test/"

expressionView : Mapping.Output ExpressionView
expressionView =
  Mapping.mirror Mappings.expressionView (baseUrl ++ "ExpressionView")

main : Signal Html
main =
  Signal.map
    (\a ->
      Html.div
        []
        [
          a |> View.viewStored View.viewExpressionView
        ]
    )
    expressionView.model

toText : String -> a -> Html
toText title a =
  Html.div
    []
    [title ++ ": " ++ (a |> toString) |> Html.text]

port expressionViewTasks : Signal (Task () ())
port expressionViewTasks =
  expressionView.tasksToRun
