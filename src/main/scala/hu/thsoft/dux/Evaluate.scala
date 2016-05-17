package hu.thsoft.dux

import hu.thsoft.dux.types._
import hu.thsoft.firebasemodel.Mapping._
import hu.thsoft.firebasemodel.Invalid
import hu.thsoft.firebasemodel.Stored

object Evaluate {

  case class Evaluation(
    storedExpression: Stored[Expression],
    result: Either[Failure, Success]
  )

  sealed trait Failure
  case class InvalidExpression(cause: Invalid) extends Failure

  type Success = Double

  def wrap[T](either: Either[Invalid, T]): Either[Failure, T] =
    either.left.map(invalid => InvalidExpression(invalid))

  def apply(storedExpression: Stored[Expression]): Evaluation = {
    val result =
      wrap(storedExpression.value).right.flatMap(expression => {
        expression match {
          case numberLiteral: NumberLiteral =>
            wrap(numberLiteral.value.value)
          case functionCall: FunctionCall => {
            for {
              functionType <- wrap(functionCall.functionType.value).right
              firstArgument <- apply(functionCall.firstArgument).result.right
              secondArgument <- apply(functionCall.secondArgument).result.right
            } yield {
              val function: ((Double, Double) => Double) =
                functionType match {
                  case Add => { _ + _ }
                  case Subtract => { _ - _ }
                  case Multiply => { _ * _ }
                  case Divide => { _ / _ }
                }
              function.apply(firstArgument, secondArgument)
            }
          }
        }
      })
    Evaluation(storedExpression, result)
  }

}