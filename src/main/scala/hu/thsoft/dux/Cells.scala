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
    fromStored(storedDouble)(double => {
      atomicContent[String](
        <.span(double),
        double.toString
      ).copy(menu =
        Some(input =>
          Try { input.toDouble }.toOption.map(newValue => {
            List(
              Command(
                input,
                s"Change to $input",
                Callback {
                  Mapping.double.set(storedDouble.firebase, newValue)
                },
                SlotId(storedDouble.firebase.toString, ContentSlot)
              )
            )
          }).getOrElse(List())
        )
      )
    })

  val functionTypeStringValues: Map[FunctionType, String] =
    Map(
      Add -> "+",
      Subtract -> "-",
      Multiply -> "*",
      Divide -> "/"
    )

  def fromFunctionType(storedFunctionType: Stored[FunctionType]): Cell[String] = {
    val contentTexts: Map[FunctionType, String] =
      Map(
        Add -> "+",
        Subtract -> "-",
        Multiply -> "·",
        Divide -> "/"
      )
    fromStored(storedFunctionType)(functionType => {
      atomicContent[String](
        <.div(
          contentTexts.getOrElse[String](functionType, errorString),
          Styles.functionType
        ),
        functionTypeStringValues.getOrElse(functionType, "")
      ).copy(menu = Some(input =>
        functionTypeStringValues.map(_.swap).get(input).map(newValue => {
          List(
            Command(
              input,
              s"Change to $input",
              Callback {
                mappings.functionType.set(storedFunctionType.firebase, newValue)
              },
              SlotId(storedFunctionType.firebase.toString, ContentSlot)
            )
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
    val cell =
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
    def sideMenu(right: Boolean): Menu[String] = {
      storedExpression.value.right.toOption.map(expression => {
        (input => {
          functionTypeStringValues.map(_.swap).get(input).map(functionType => {
            val storedFunctionType =
              Remote(storedExpression.firebase, Right(functionType))
            val storedFirstArgument =
              if (right) {
                Remote(storedExpression.firebase, Right(expression))
              } else {
                val storedValue = Remote(storedExpression.firebase, Right(0.0))
                Remote(storedExpression.firebase, Right(NumberLiteral(value = storedValue)))
              }
            val storedSecondArgument =
              if (right) {
                val storedValue = Remote(storedExpression.firebase, Right(0.0))
                Remote(storedExpression.firebase, Right(NumberLiteral(value = storedValue)))
              } else {
                Remote(storedExpression.firebase, Right(expression))
              }
            val newValue =
              FunctionCall(
                functionType = storedFunctionType,
                firstArgument = storedFirstArgument,
                secondArgument = storedSecondArgument
              )
            val childKey =
              if (right) mappings.secondArgumentKey else mappings.firstArgumentKey
            List(
              Command(
                if (right) s"□${input}_" else s"_${input}□",
                s"Apply $input",
                Callback {
                  mappings.expression.set(storedExpression.firebase, newValue)
                },
                SlotId(storedExpression.firebase.child(childKey).toString, ContentSlot)
              )
            )
          }).getOrElse(List())
        })
      })
    }
    val leftMenu = sideMenu(false)
    val rightMenu = sideMenu(true)
    val cellWithLeftMenu = setLeftMenu(cell, leftMenu)
    val cellWithBothMenus = setRightMenu(cellWithLeftMenu, rightMenu)
    cellWithBothMenus
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