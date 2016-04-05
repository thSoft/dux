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

object Cells {

  val errorString = "⚠"

  def stored[T](stored: Stored[T])(view: T => CellContent[String]): Cell[String] = {
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

  def double(storedDouble: Stored[Double]): Cell[String] =
    stored(storedDouble)(double =>
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

  def functionType(storedFunctionType: Stored[FunctionType]): Cell[String] = {
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
    stored(storedFunctionType)(functionType => {
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

  def expression(storedExpression: Stored[Expression]): Cell[String] = {
    storedExpression.value.right.map(expression =>
      expression match {
      case expression: NumberLiteral => numberLiteral(Remote(storedExpression.firebase, Right(expression)))
      case expression: FunctionCall => functionCall(Remote(storedExpression.firebase, Right(expression)))
      }
    ).fold(left => {
      Cell(storedExpression.firebase.toString, atomicContent(<.span(errorString), ""))
    }, right => right)
  }

  def numberLiteral(storedNumberLiteral: Stored[NumberLiteral]): Cell[String] =
    stored(storedNumberLiteral)(numberLiteral => {
      compositeContent(
        List(double(numberLiteral.value)),
        Styles.expression
      )
    })

  def functionCall(storedFunctionCall: Stored[FunctionCall]): Cell[String] =
    stored(storedFunctionCall)(functionCall => {
      compositeContent(
        List(
          expression(functionCall.firstArgument),
          functionType(functionCall.functionType),
          expression(functionCall.secondArgument)
        ),
        Styles.expression
      )
    })

  def expressionView(storedExpressionView: Stored[ExpressionView]): Cell[String] =
    stored(storedExpressionView)(expressionView => {
      compositeContent(
        List(
          stored(expressionView.expression)(storedExpression =>
            compositeContent(expression(storedExpression))
          )
        ),
        Styles.expressionView
      )
    })

  def workspace(storedWorkspace: Stored[Workspace]): Cell[String] =
    stored(storedWorkspace)(workspace => {
      compositeContent(
        stored(workspace.views)(views =>
          compositeContent(
            views.map(view =>
              expressionView(view)
            )
          )
        )
      )
    })

}