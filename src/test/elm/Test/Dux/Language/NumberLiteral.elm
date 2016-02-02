module Test.Dux.Environment.Language.NumberLiteral where

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

output : Mapping.Output NumberLiteral
output =
  Mapping.mirror Mappings.numberLiteral "https://thsoft.firebaseio.com/DUX/test/NumberLiteral"

port tasks : Signal (Task () ())
port tasks =
  output.tasksToRun
