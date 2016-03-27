package hu.thsoft.dux

import hu.thsoft.firebasemodel.Mapping._
import hu.thsoft.firebasemodel.Remote

package object types {

  sealed trait FunctionType
  case object Add extends FunctionType
  case object Subtract extends FunctionType
  case object Multiply extends FunctionType
  case object Divide extends FunctionType

  sealed trait Expression
  case class NumberLiteral(
    value: Stored[Double]
  ) extends Expression
  case class FunctionCall(
    functionType: Stored[FunctionType],
    firstArgument: Stored[Expression],
    secondArgument: Stored[Expression]
  ) extends Expression

  case class ExpressionView(
    expression: Stored[Remote[Expression]]
  )

  case class Workspace(
    views: Stored[Many[ExpressionView]]
  )

}