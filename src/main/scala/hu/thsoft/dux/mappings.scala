package hu.thsoft.dux

import hu.thsoft.firebasemodel.Mapping
import hu.thsoft.dux.types._
import scalaz.syntax.apply._
import hu.thsoft.firebasemodel.Field
import hu.thsoft.firebasemodel.Alternative

package object mappings {

  lazy val functionType: Mapping[FunctionType] = {
    Mapping.choice(
      Alternative("add", () => Mapping.always(Add)),
      Alternative("subtract", () => Mapping.always(Subtract)),
      Alternative("multiply", () => Mapping.always(Multiply)),
      Alternative("divide", () => Mapping.always(Divide))
    )
  }

  lazy val expression: Mapping[Expression] = {
    Mapping.choice(
      Alternative("numberLiteral", () => numberLiteral),
      Alternative("functionCall", () => functionCall)
    )
  }

  lazy val numberLiteral: Mapping[NumberLiteral] =
    Mapping.record(NumberLiteral)(
      Field(valueKey, Mapping.double, _.value)
    )
  val valueKey = "value"

  lazy val functionCall: Mapping[FunctionCall] =
    Mapping.record(FunctionCall)(
      Field(functionTypeKey, functionType, _.functionType),
      Field(firstArgumentKey, expression, _.firstArgument),
      Field(secondArgumentKey, expression, _.secondArgument)
    )
  val functionTypeKey = "functionType"
  val firstArgumentKey = "firstArgument"
  val secondArgumentKey = "secondArgument"

  lazy val expressionView: Mapping[ExpressionView] =
    Mapping.record(ExpressionView)(
      Field(expressionKey, Mapping.reference(expression), _.expression)
    )
  val expressionKey = "expression"

  lazy val workspace: Mapping[Workspace] =
    Mapping.record(Workspace)(
      Field(viewsKey, Mapping.list(expressionView), _.views)
    )
  val viewsKey = "views"

}