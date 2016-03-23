package hu.thsoft.dux

import japgolly.scalajs.react.vdom.prefix_<^._
import hu.thsoft.dux.types._
import japgolly.scalajs.react.ReactElement

package object views {

  lazy val cell =
    List(
      ^.display.`inline-block`,
      ^.padding := "1px",
      ^.margin := "1px",
      ^.border := "1px solid gray",
      ^.borderRadius := "2px"
    )

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
      numberLiteral.value,
      cell
    )
  
  def functionCall(functionCall: FunctionCall): ReactElement =
    <.div(
      expression(functionCall.firstArgument),
      functionType(functionCall.functionType),
      expression(functionCall.secondArgument),
      cell
    )

}