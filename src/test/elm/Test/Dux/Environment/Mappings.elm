module Test.Dux.Environment.Mappings where

import Task exposing (Task)
import Html exposing (Html)
import Dux.Environment.Types exposing (..)
import Dux.Environment.Mappings as Mappings
import Dux.Environment.View as View
import FirebaseModel.Mapping as Mapping

baseUrl : String
baseUrl = "https://thsoft.firebaseio.com/DUX/test/"

workspace : Mapping.Output Workspace
workspace =
  Mapping.mirror Mappings.workspace (baseUrl ++ "Workspace")

main : Signal Html
main =
  Signal.map
    (\a ->
      Html.div
        []
        [
          a |> View.viewStored View.viewWorkspace
        ]
    )
    workspace.model

port workspaceTasks : Signal (Task () ())
port workspaceTasks =
  workspace.tasksToRun
