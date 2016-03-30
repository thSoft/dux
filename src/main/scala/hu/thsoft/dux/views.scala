package hu.thsoft.dux

import japgolly.scalajs.react.vdom.prefix_<^._
import hu.thsoft.dux.types._
import japgolly.scalajs.react.ReactElement
import hu.thsoft.firebasemodel.Mapping._

package object views {

  def stored[T](stored: Stored[T])(view: T => ReactElement): ReactElement =
    stored.value match {
      case Left(invalid) =>
        <.a(
          "⚠",
          ^.href := stored.firebase.toString,
          ^.target := "_blank",
          ^.title := s"Expected ${invalid.expectedTypeName} but got ${invalid.json}"
        )
      case Right(value) => view(value)
    }

  lazy val cell =
    List(
      ^.display.`inline-block`,
      ^.padding := "1px",
      ^.margin := "1px",
      ^.border := "1px solid gray",
      ^.borderRadius := "2px"
    )

  def double(double: Stored[Double]): ReactElement =
    stored(double)(value => <.span(value))

  def functionType(functionType: FunctionType): ReactElement =
    <.div(
      functionType match {
        case Add => "+"
        case Subtract => "-"
        case Multiply => "*"
        case Divide => "*"
      },
      cell
    )

  def expression(expression: Expression): ReactElement =
    expression match {
      case numberLiteralExpression: NumberLiteral => numberLiteral(numberLiteralExpression)
      case functionCallExpression: FunctionCall => functionCall(functionCallExpression)
    }

  def numberLiteral(numberLiteral: NumberLiteral): ReactElement =
    <.div(
      double(numberLiteral.value),
      cell
    )

  def functionCall(functionCall: FunctionCall): ReactElement =
    <.div(
      stored(functionCall.firstArgument)(expression),
      stored(functionCall.functionType)(functionType),
      stored(functionCall.secondArgument)(expression),
      cell
    )

  def expressionView(expressionView: ExpressionView): ReactElement =
    <.div(
      stored(expressionView.expression)(storedExpression =>
        stored(storedExpression)(expression)
      )
    )

  def workspace(workspace: Workspace): ReactElement =
    <.div(
      stored(workspace.views)(views =>
        <.div(
          views.map(view =>
            stored(view)(expressionView)
          )
        )
      )
    )

}