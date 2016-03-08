module Test.Dux.Environment.Cells where

import Task exposing (Task)
import Html exposing (Html)
import Dux.Language.Types exposing (..)
import Dux.Language.Mappings
import Dux.Environment.Cells exposing (CellId)
import Component exposing (Component, OutputWithAddress)
import FirebaseModel.Mapping as Mapping
import Cells.Types exposing (Cell, Editor, Action(..))
import Cells.Editor as Editor
import Cells.Cell as Cell

baseUrl : String
baseUrl =
  "https://thsoft.firebaseio.com/DUX/test/"

url : String
url =
  baseUrl ++ "NumberLiteral"

numberLiteral : Mapping.Output NumberLiteral
numberLiteral =
  Mapping.mirror Dux.Language.Mappings.numberLiteral url

editor : OutputWithAddress (Editor CellId) (Action CellId)
editor =
  Editor.component (Dux.Environment.Cells.loading url)
  |> Component.start'

main : Signal Html
main =
  Signal.map2
    (\editorValue numberLiteralValue ->
      let result =
            Cell.view
              editor.address
              editorValue
              cell
          cell =
            numberLiteralValue |> Dux.Environment.Cells.viewStored (Dux.Environment.Cells.viewNumberLiteral editorValue.combobox.inputText)
      in result
    )
    editor.model
    numberLiteral.model

port editorTasks : Signal (Task () ())
port editorTasks =
  editor.tasks

port numberLiteralTasks : Signal (Task () ())
port numberLiteralTasks =
  numberLiteral.tasksToRun
