package hu.thsoft.dux

import hu.thsoft.firebasemodel.Mapping
import hu.thsoft.dux.types._
import scalaz.syntax.apply._

package object mappings {

  lazy val functionType: Mapping[FunctionType] =
    Mapping.map(Mapping.string)(_ match {
      case "add" => Add
      case "subtract" => Subtract
      case "multiply" => Multiply
      case "divide" => Divide
    })

  lazy val expression: Mapping[Expression] =
    Mapping.choice(
      "numberLiteral" -> (() => numberLiteral),
      "functionCall" -> (() => functionCall)
    )

  lazy val numberLiteral: Mapping[NumberLiteral] =
    Mapping.map(Mapping.field("value", Mapping.double))(NumberLiteral)

  lazy val functionCall: Mapping[FunctionCall] =
    (
      Mapping.field("functionType", functionType) |@|
      Mapping.field("firstArgument", expression) |@|
      Mapping.field("secondArgument", expression)
    )(FunctionCall)

}