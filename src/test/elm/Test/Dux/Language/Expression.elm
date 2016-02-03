module Test.Dux.Environment.Language.FunctionType where

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

output : Mapping.Output Expression
output =
  Mapping.mirror Mappings.expression "https://thsoft.firebaseio.com/DUX/test/Expression"

port tasks : Signal (Task () ())
port tasks =
  output.tasksToRun
