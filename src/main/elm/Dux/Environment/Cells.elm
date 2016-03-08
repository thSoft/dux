module Dux.Environment.Cells where

import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import FirebaseModel.Mapping exposing (..)
import TaskUtil
import Dux.Language.Types exposing (..)
import Dux.Environment.Types exposing (..)
import Dux.Language.Mappings
import Cells.Types exposing (Cell, CellContent(..))
import Cells.Cell as Cell

type alias CellId =
  {
    modelUrl: String,
    childId: ChildId
  }

type ChildId =
  NoChild
{-
viewWorkspace : Workspace -> Cell CellId
viewWorkspace workspace =
  workspace.views |> viewStored (\views ->
    {
      id =
        {
          modelUrl =
            workspace.url,
          childId =
            NoChild
        },
      content =
        Composite {
          children =
            List.map views viewExpressionView
        },
      leftMenu =
        Nothing,
      rightMenu =
        Nothing
    }
  )

viewExpressionView : ExpressionView -> Cell CellId
viewExpressionView expressionView =
  div
    []
    [expressionView.expression |> viewStored (viewReference (viewStored viewExpression))]
-}
emptyId : CellId
emptyId =
  {
    modelUrl =
      "",
    childId =
      NoChild
  }

viewStored : (String -> a -> Cell CellId) -> Stored a -> Cell CellId
viewStored view stored =
  let result =
        { cell | id = idWithUrl }
      cellId =
        cell.id
      idWithUrl =
        { cellId | modelUrl = stored.url }
      cell =
        case stored.data of
          Ok model ->
            view stored.url model
          Err error ->
            case error of
              Loading ->
                loading stored.url
                |> Cell.htmlCell emptyId ""
              DecodingError decodingError ->
                img
                  [
                    src errorImage,
                    title <| "Decoding failed: " ++ decodingError
                  ]
                  []
                |> Cell.htmlCell emptyId ""
              SubscriptionError subscriptionError ->
                let result' =
                      img
                        [
                          src image,
                          title titleText
                        ]
                        []
                      |> Cell.htmlCell emptyId ""
                    image =
                      case subscriptionError of
                        NoSubscription ->
                          loadingImage
                        _ ->
                          errorImage
                    titleText =
                      case subscriptionError of
                        NoSubscription ->
                          "Not subscribed to " ++ stored.url
                        ElmFireError elmFireError ->
                          "Subscription to " ++ stored.url ++ " failed: " ++ (elmFireError |> toString)
                        ElmFireCancellation cancellation ->
                          "Subscription to " ++ stored.url ++ " cancelled: " ++ (cancellation |> toString)
                in result'
  in result

loading : String -> Html
loading url =
  img
    [
      src loadingImage,
      title <| "Loading..." ++ url
    ]
    []

loadingImage : String
loadingImage =
  "http://www.ajaxload.info/images/exemples/6.gif"

errorImage : String
errorImage =
  "http://findicons.com/files/icons/1689/splashy/16/error.png"
{-
viewReference : (Stored a -> Cell CellId) -> Reference a -> Cell CellId
viewReference view reference =
  reference.get () |> view

viewExpression : Expression -> Cell CellId
viewExpression expression =
  span
    [
      style [
        ("display", "inline-block"),
        ("border", "1px solid lightGray"),
        ("border-radius", "4px"),
        ("padding", "1px"),
        ("margin", "1px")
      ]
    ]
    [
      case expression of
        NumberLiteralExpression numberLiteral ->
          viewNumberLiteral numberLiteral
        FunctionCallExpression functionCall ->
          viewFunctionCall functionCall
    ]
-}
viewNumberLiteral : String -> String -> NumberLiteral -> Cell CellId
viewNumberLiteral inputText url numberLiteral =
  numberLiteral.value |> viewStored (\valueUrl value ->
    {
      id =
        emptyId,
      content =
        Atomic {
          html =
            value |> toString |> text,
          text =
            value |> toString,
          menu =
            Just [
              inputText
              |> String.toFloat
              |> Result.toMaybe
              |> Maybe.map (\float ->
                {
                  label =
                    "Set to " ++ inputText,
                  task =
                    FirebaseModel.Mapping.set
                      Dux.Language.Mappings.numberLiteral
                      {
                        value =
                          {
                            data =
                              Ok float,
                            url =
                              valueUrl
                          }
                      }
                      url
                    |> TaskUtil.swallowError "set failed"
                }
              )
              |> Maybe.withDefault {
                label =
                  "Please enter a number",
                task =
                  Task.succeed ()
              }
            ]
        },
      leftMenu =
        Nothing,
      rightMenu =
        Nothing
    }
  )
{-
viewFunctionCall : FunctionCall -> Cell CellId
viewFunctionCall functionCall =
  span
    []
    [
      functionCall.firstArgument |> viewStored viewExpression,
      functionCall.functionType |> viewStored viewFunctionType,
      functionCall.secondArgument |> viewStored viewExpression
    ]
-}
viewFunctionType : FunctionType -> Cell CellId
viewFunctionType functionType =
  case functionType of
    Add ->
      Cell.textCell emptyId "+"
    Subtract ->
      Cell.textCell emptyId "-"
    Multiply ->
      Cell.htmlCell emptyId "*" ("⋅" |> text)
    Divide ->
      Cell.textCell emptyId "/" -- TODO horizontal bar
