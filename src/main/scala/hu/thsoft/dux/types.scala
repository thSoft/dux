package hu.thsoft.dux

package object types {

  sealed trait FunctionType
  case object Add extends FunctionType
  case object Subtract extends FunctionType
  case object Multiply extends FunctionType
  case object Divide extends FunctionType

  sealed trait Expression
  case class NumberLiteral(
    value: Double
  ) extends Expression
  case class FunctionCall(
    functionType: FunctionType,
    firstArgument: Expression,
    secondArgument: Expression
  ) extends Expression

}