package hu.thsoft.dux

import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.raw.Node
import hu.thsoft.dux.types._
import hu.thsoft.firebasemodel.Mapping._
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.ReactEventAliases
import japgolly.scalajs.react.SyntheticCompositionEvent
import japgolly.scalajs.react.SyntheticEvent
import japgolly.scalajs.react.SyntheticKeyboardEvent
import japgolly.scalajs.react.SyntheticMouseEvent
import japgolly.scalajs.react.vdom.prefix_<^._
import monifu.reactive.Channel
import monifu.reactive.Observer
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import org.scalajs.dom.ext.KeyValue
import hu.thsoft.dux.cells.EditorState
import hu.thsoft.dux.cells._
import scala.util.Try
import hu.thsoft.firebasemodel.Mapping
import hu.thsoft.firebasemodel.Remote
import hu.thsoft.dux.Evaluate.Evaluation

object Cells {

  val errorString = "⚠"

  def fromStored[T](stored: Stored[T])(view: T => CellContent[String]): Cell[String] = {
    val url = stored.firebase.toString
    val content: CellContent[String] =
      stored.value match {
        case Left(invalid) =>
          atomicContent(
            <.a(
              errorString,
              ^.href := url,
              ^.target := "_blank",
              ^.title := s"Expected ${invalid.expectedTypeName} but got ${invalid.json}"
            ),
            ""
          )
        case Right(value) =>
          view(value)
      }
    Cell(url, content)
  }

  def fromDouble(storedDouble: Stored[Double]): Cell[String] =
    fromStored(storedDouble)(double =>
      atomicContent(
        <.span(double),
        double.toString
      ).copy(menu = Some(input =>
        Try { input.toDouble }.toOption.map(newValue => {
          List(
            Command(input, s"Change to $input", Callback {
              Mapping.double.set(storedDouble.firebase, newValue)
            })
          )
        }).getOrElse(List())
      ))
    )

  def fromFunctionType(storedFunctionType: Stored[FunctionType]): Cell[String] = {
    val stringValues: Map[FunctionType, String] =
      Map(
        Add -> "+",
        Subtract -> "-",
        Multiply -> "*",
        Divide -> "/"
      )
    val contentTexts: Map[FunctionType, String] =
      Map(
        Add -> "+",
        Subtract -> "-",
        Multiply -> "·",
        Divide -> "/"
      )
    fromStored(storedFunctionType)(functionType => {
      atomicContent(
        <.div(
          contentTexts.getOrElse[String](functionType, errorString),
          Styles.functionType
        ),
        stringValues.getOrElse(functionType, "")
      ).copy(menu = Some(input =>
        stringValues.map(_.swap).get(input).map(newValue => {
          List(
            Command(input, s"Change to $input", Callback {
              mappings.functionType.set(storedFunctionType.firebase, newValue)
            })
          )
        }).getOrElse(List())
      ))
    })
  }

  def showEvaluation(evaluation: Evaluation): String = {
    evaluation.result.fold(
      failure => {
        failure.toString
      },
      success => {
        success.toString
      }
    )
  }

  def fromExpression(storedExpression: Stored[Expression]): Cell[String] = {
    fromStored(storedExpression)(expression => {
      val evaluation = Evaluate(storedExpression)
      val tagMod =
        (^.title := showEvaluation(evaluation)) +
        Styles.expression
      val children =
        expression match {
          case numberLiteral: NumberLiteral =>
            List(fromDouble(numberLiteral.value))
          case functionCall: FunctionCall =>
            List(
              fromExpression(functionCall.firstArgument),
              fromFunctionType(functionCall.functionType),
              fromExpression(functionCall.secondArgument)
            )
        }
      compositeContent(children, tagMod)
    })
  }

  def fromExpressionView(storedExpressionView: Stored[ExpressionView]): Cell[String] =
    fromStored(storedExpressionView)(expressionView => {
      compositeContent(
        List(
          fromStored(expressionView.expression)(storedExpression =>
            compositeContent(fromExpression(storedExpression))
          )
        ),
        Styles.expressionView
      )
    })

  def fromWorkspace(storedWorkspace: Stored[Workspace]): Cell[String] =
    fromStored(storedWorkspace)(workspace => {
      compositeContent(
        fromStored(workspace.views)(views =>
          compositeContent(
            views.map(view =>
              fromExpressionView(view)
            )
          )
        )
      )
    })

}