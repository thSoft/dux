module Test.Dux.Environment.Language.Mappings where

import Task exposing (Task)
import Html exposing (Html)
import Dux.Language.Types exposing (..)
import Dux.Language.Mappings as Mappings
import FirebaseModel.Mapping as Mapping

main : Signal Html
main =
  output.model |> Signal.map (\model ->
    model |> toString |> Html.text
  )

output : Mapping.Output FunctionType
output =
  Mapping.mirror Mappings.functionType "https://thsoft.firebaseio.com/DUX/test/FunctionType"

port tasks : Signal (Task () ())
port tasks =
  output.tasksToRun
