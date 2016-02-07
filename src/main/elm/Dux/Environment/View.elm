module Dux.Environment.View where

import Html exposing (..)
import Html.Attributes exposing (..)
import FirebaseModel.Mapping exposing (..)
import Dux.Language.Types exposing (..)
import Dux.Environment.Types exposing (..)

viewExpressionView : ExpressionView -> Html
viewExpressionView expressionView =
  expressionView.expression |> viewStored (viewReference (viewStored viewExpression))

viewStored : (a -> Html) -> Stored a -> Html
viewStored view stored =
  span
    [title stored.url]
    [
      case stored.data of
        Ok model ->
          view model
        Err error ->
          case error of
            Loading ->
              img
                [
                  src loadingImage,
                  title <| "Loading..." ++ stored.url
                ]
                []
            DecodingError decodingError ->
              img
                [
                  src errorImage,
                  title <| "Decoding failed: " ++ decodingError
                ]
                []
            SubscriptionError subscriptionError ->
              let result =
                    img
                      [
                        src image,
                        title titleText
                      ]
                      []
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
                        "Subscription failed to " ++ stored.url ++ ": " ++ (elmFireError |> toString)
                      ElmFireCancellation cancellation ->
                        "Subscription cancelled " ++ stored.url ++ ": " ++ (cancellation |> toString)
              in result
    ]

loadingImage : String
loadingImage =
  "http://www.ajaxload.info/images/exemples/6.gif"

errorImage : String
errorImage =
  "http://findicons.com/files/icons/1689/splashy/16/error.png"

viewReference : (Stored a -> Html) -> Reference a -> Html
viewReference view reference =
  reference.get () |> view

viewExpression : Expression -> Html
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

viewNumberLiteral : NumberLiteral -> Html
viewNumberLiteral numberLiteral =
  numberLiteral.value |> viewStored (\value ->
    value |> toString |> text
  )

viewFunctionCall : FunctionCall -> Html
viewFunctionCall functionCall =
  span
    []
    [
      functionCall.firstArgument |> viewStored viewExpression,
      functionCall.functionType |> viewStored viewFunctionType,
      functionCall.secondArgument |> viewStored viewExpression
    ]

viewFunctionType : FunctionType -> Html
viewFunctionType functionType =
  text <| case functionType of
    Add ->
      "+"
    Subtract ->
      "-"
    Multiply ->
      "*"
    Divide ->
      "/"
