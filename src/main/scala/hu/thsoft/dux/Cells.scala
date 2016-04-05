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

object Cells {

  def stored[T](stored: Stored[T])(view: T => CellContent[String]): Cell[String] = {
    val url = stored.firebase.toString
    val content: CellContent[String] =
      stored.value match {
        case Left(invalid) =>
          atomicContent(
            <.a(
              "⚠",
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

  def double(double: Stored[Double]): Cell[String] =
    stored(double)(value =>
      atomicContent(
        <.span(value),
        value.toString
      ).copy(menu = Some(input =>
        Try { input.toDouble }.toOption.map(newValue => {
          List(
            Command(input, s"Change to $input", Callback {
              Mapping.double.set(double.firebase, newValue)
            })
          )
        }).getOrElse(List())
      ))
    )

  def functionType(functionType: FunctionType): CellContent[String] = {
    val contentText =
      functionType match {
      case Add => "+"
      case Subtract => "-"
      case Multiply => "·"
      case Divide => "/"
      }
    val stringValue =
      functionType match {
      case Add => "+"
      case Subtract => "-"
      case Multiply => "*"
      case Divide => "/"
      }
    atomicContent(
      <.div(
        contentText,
        Styles.functionType
      ),
      stringValue
    )
  }

  def expression(expression: Expression): CellContent[String] = {
    val compositeContent =
        expression match {
        case expression: NumberLiteral => numberLiteral(expression)
        case expression: FunctionCall => functionCall(expression)
    }
    compositeContent.copy(tagMod = Styles.expression)
  }

  def numberLiteral(numberLiteral: NumberLiteral): CompositeContent[String] =
    compositeContent(double(numberLiteral.value))

  def functionCall(functionCall: FunctionCall): CompositeContent[String] =
    compositeContent(
      stored(functionCall.firstArgument)(expression),
      stored(functionCall.functionType)(functionType),
      stored(functionCall.secondArgument)(expression)
    )

  def expressionView(expressionView: ExpressionView): CellContent[String] =
    compositeContent(
      List(
        stored(expressionView.expression)(storedExpression =>
          compositeContent(stored(storedExpression)(expression))
        )
      ),
      Styles.expressionView
    )

  def workspace(workspace: Workspace): CellContent[String] =
    compositeContent(
      stored(workspace.views)(views =>
        compositeContent(
          views.map(view =>
            stored(view)(expressionView)
          )
        )
      )
    )

}