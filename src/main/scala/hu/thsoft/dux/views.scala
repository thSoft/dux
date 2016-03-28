package hu.thsoft.dux

import japgolly.scalajs.react.vdom.prefix_<^._
import hu.thsoft.dux.types._
import japgolly.scalajs.react.ReactElement
import hu.thsoft.firebasemodel.Mapping._

package object views {

  def stored[T](view: T => ReactElement)(stored: Stored[T]): ReactElement =
    stored.value match {
      case Left(invalid) =>
        <.img(
          ^.src := "http://findicons.com/files/icons/1689/splashy/16/error.png",
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
    stored((value: Double) => <.span(value))(double)

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
      stored(expression)(functionCall.firstArgument),
      stored(functionType)(functionCall.functionType),
      stored(expression)(functionCall.secondArgument),
      cell
    )

  def expressionView(expressionView: ExpressionView): ReactElement =
    <.div(
      stored((storedExpression: Stored[Expression]) => stored(expression)(storedExpression))(expressionView.expression)
    )

  def workspace(workspace: Workspace): ReactElement =
    <.div(
      stored((views: Many[ExpressionView]) =>
        <.div(
          views.map((view: Stored[ExpressionView]) =>
            stored(expressionView)(view)
          )
        )
      )(workspace.views)
    )

}