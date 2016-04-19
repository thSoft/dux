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
import hu.thsoft.firebase.Firebase
import scala.concurrent.ExecutionContext

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

  def fromDouble(storedDouble: Stored[Double]): Cell[String] = {
    val getCommands =
      (input: String) =>
        Try { input.toDouble }.toOption.map(newValue => {
          List(
            Command[String](
              text = input,
              description = s"Change to $input",
              callback = Callback {
                Mapping.double.set(storedDouble.firebase, newValue)
              },
              navigation = NavigateRight
            )
          )
        }).getOrElse(List())
    fromStored(storedDouble)(double => {
      atomicContent[String](
        <.span(double),
        double.toString
      ).copy(menu =
        Some(MenuContent(
          getCommands = getCommands,
          deleteCommand = command(Callback.empty, NoNavigation)
        ))
      )
    })
  }

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
    val getCommands =
      (input: String) =>
        functionTypeStringValues.map(_.swap).get(input).map(newValue => {
          List(
            Command[String](
              text = input,
              description = s"Change to $input",
              callback = Callback {
                mappings.functionType.set(storedFunctionType.firebase, newValue)
              },
              navigation = NavigateRight
            )
          )
        }).getOrElse(List())
    fromStored(storedFunctionType)(functionType => {
      atomicContent[String](
        <.div(
          contentTexts.getOrElse[String](functionType, errorString),
          Styles.functionType
        ),
        functionTypeStringValues.getOrElse(functionType, "")
      ).copy(menu =
        Some(MenuContent(
          getCommands = getCommands,
          deleteCommand = command(Callback.empty, NoNavigation)
        ))
      )
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
        val getCommands =
          (input: String) => {
            functionTypeStringValues.map(_.swap).get(input).map(functionType => {
              val storedFunctionType =
                Remote(storedExpression.firebase, Right(functionType))
              val storedFirstArgument =
                if (right) {
                  Remote(storedExpression.firebase, Right(expression))
                } else {
                  Remote(storedExpression.firebase, Right(defaultExpression(0)))
                }
              val storedSecondArgument =
                if (right) {
                  Remote(storedExpression.firebase, Right(defaultExpression(0)))
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
              val expressionValueId =
                Mapping.valueChild(storedExpression.firebase)
              List(
                Command[String](
                  text = if (right) s"□ ${input} _" else s"_ ${input} □",
                  description = s"Apply $input",
                  callback = Callback {
                    mappings.expression.set(storedExpression.firebase, newValue)
                  },
                  navigation =
                    NavigateTo(SlotId(
                      cellId =
                        Mapping.valueChild(expressionValueId.child(childKey)).child(mappings.valueKey).toString, // XXX this is hardcoded and unchecked
                      slotType = ContentSlot
                    ))
                )
              )
            }).getOrElse(List())
          }
        val deleteCallback =
          Callback {
            mappings.expression.set(storedExpression.firebase, defaultExpression(0))
          }
        MenuContent(
          getCommands = getCommands,
          deleteCommand =
            command(deleteCallback,
              NavigateTo(SlotId(
                cellId = Mapping.valueChild(storedExpression.firebase).child(mappings.valueKey).toString,
                slotType = ContentSlot
              ))
            )
        )
      })
    }
    val leftMenu = sideMenu(false)
    val rightMenu = sideMenu(true)
    val cellWithLeftMenu = setLeftMenu(cell, leftMenu)
    val cellWithBothMenus = setRightMenu(cellWithLeftMenu, rightMenu)
    cellWithBothMenus
  }

  def defaultExpression(double: Double): Expression = {
    NumberLiteral(value = wrap(double))
  }

  def wrap[T](value: T): Stored[T] = {
    Remote(new Firebase("https://thsoft.firebaseio.com"), Right(value))
  }

  def fromExpressionView(storedExpressionView: Stored[ExpressionView], storedWorkspace: Stored[Workspace]): Cell[String] =
    fromStored(storedExpressionView)(expressionView => {
      val content = compositeContent(
        List(
          fromStored(expressionView.expression)(storedExpression =>
            compositeContent(fromExpression(storedExpression))
          )
        ),
        Styles.expressionView
      )
      val menu =
        Some(MenuContent(
          getCommands = expressionViewListGetCommands(storedWorkspace),
          deleteCommand = command(Callback {
            storedExpressionView.firebase.remove
          }, NavigateRight)
        ))
      content.copy(leftMenu = menu, rightMenu = menu)
    })

  def expressionViewListGetCommands(storedWorkspace: Stored[Workspace]): String => List[Command[String]] =
    input => {
      Try { input.toDouble }.toOption.map(newValue => {
        List(
          Command[String](
            text = input,
            description = "Insert number literal",
            callback = CallbackTo {
              implicit val executionContext = ExecutionContext.global
              val expressionsParent = new Firebase("https://thsoft.firebaseio.com/DUX/test/Expressions")
              for (
                newStoredExpression <- mappings.expression.push(expressionsParent, defaultExpression(newValue));
                expressionViewsParent = storedWorkspace.firebase.child(mappings.viewsKey);
                expressionView = ExpressionView(expression = wrap(newStoredExpression));
                newStoredExpressionView <- mappings.expressionView.push(expressionViewsParent, expressionView)
              ) yield newStoredExpressionView
            },
            navigation = NoNavigation
          )
        )
      }).getOrElse(List())
    }

  def fromWorkspace(storedWorkspace: Stored[Workspace]): Cell[String] =
    fromStored(storedWorkspace)(workspace => {
      if (workspace.views.value.isLeft) {
        atomicContent[String](<.span("..."), "").copy(menu =
          Some(MenuContent(
            getCommands = expressionViewListGetCommands(storedWorkspace),
            deleteCommand = command(Callback.empty, NoNavigation)
          ))
        )
      } else {
        compositeContent(
          List(fromStored(workspace.views)(views => {
            compositeContent(
              views.map(view =>
                fromExpressionView(view, storedWorkspace)
              )
            )
          })),
          Styles.workspace
        )
      }
    })

}