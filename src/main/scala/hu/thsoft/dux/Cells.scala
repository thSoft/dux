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
              callback = navigator => Callback {
                Mapping.double.set(storedDouble.firebase, newValue)
                navigator.navigateRight
              }
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
          deleteCommand = nopCommand[String]
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
              callback = navigator => Callback {
                mappings.functionType.set(storedFunctionType.firebase, newValue)
                navigator.navigateRight
              }
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
          deleteCommand = nopCommand[String]
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

  def fromExpression(storedExpression: Stored[Expression], enclosingExpression: Option[Stored[Expression]]): Cell[String] = {
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
            case functionCall: FunctionCall => {
              (if (enclosingExpression.isDefined) List(Cell(id = "(", content = atomicContent("("))) else List()) ++
              List(
                fromExpression(functionCall.firstArgument, Some(storedExpression)),
                fromFunctionType(functionCall.functionType),
                fromExpression(functionCall.secondArgument, Some(storedExpression))
              ) ++
              (if (enclosingExpression.isDefined) List(Cell(id = ")", content = atomicContent(")"))) else List())
            }
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
                  callback = navigator => Callback {
                    mappings.expression.set(storedExpression.firebase, newValue)
                    navigator.navigateTo(SlotId(
                      cellId =
                        Mapping.valueChild(expressionValueId.child(childKey)).child(mappings.valueKey).toString, // XXX this is hardcoded and unchecked
                      slotType = ContentSlot
                    ))
                  }
                )
              )
            }).getOrElse(List())
          }
        val deleteCallback =
          (navigator: Navigator[String]) => Callback {
            mappings.expression.set(storedExpression.firebase, defaultExpression(0))
            navigator.navigateTo(SlotId(
              cellId = Mapping.valueChild(storedExpression.firebase).child(mappings.valueKey).toString,
              slotType = ContentSlot
            ))
          }
        MenuContent(
          getCommands = getCommands,
          deleteCommand = command(deleteCallback)
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
            compositeContent(fromExpression(storedExpression, None))
          )
        ),
        Styles.expressionView
      )
      val menu =
        Some(MenuContent(
          getCommands = input => List(),
          deleteCommand = command((navigator: Navigator[String]) => Callback {
            for (
              expressionView <- storedExpressionView.value.right;
              storedExpression <- expressionView.expression.value.right
            ) {
              storedExpression.firebase.remove
            }
            storedExpressionView.firebase.remove
            // TODO navigate
          })
        ))
      content.copy(leftMenu = menu, rightMenu = menu)
    })

  def fromWorkspace(storedWorkspace: Stored[Workspace]): Cell[String] = {
    val getCommands: String => List[Command[String]] =
    input => {
      Try { input.toDouble }.toOption.map(newValue => {
        List(
          Command[String](
            text = input,
            description = "Add number literal",
            callback = navigator => CallbackTo {
              implicit val executionContext = ExecutionContext.global
              val expressionsParent = new Firebase("https://thsoft.firebaseio.com/DUX/test/Expressions")
              val newStoredExpressionFuture = for (
                newStoredExpression <- mappings.expression.push(expressionsParent, defaultExpression(newValue));
                expressionViewsParent = storedWorkspace.firebase.child(mappings.viewsKey);
                expressionView = ExpressionView(expression = wrap(newStoredExpression));
                newStoredExpressionView <- mappings.expressionView.push(expressionViewsParent, expressionView)
              ) yield newStoredExpression
              newStoredExpressionFuture.foreach(newStoredExpression =>
                navigator.navigateTo(SlotId(
                  cellId = newStoredExpression.firebase.toString,
                  slotType = RightSlot
                ))
              )
            }
          )
        )
      }).getOrElse(List())
    }
    fromStored(storedWorkspace)(workspace => {
      compositeContent(
        List(fromStored(workspace.views)(views => {
          compositeContent(
            views.map(view =>
              fromExpressionView(view, storedWorkspace)
            )
          )
        })) :+ (
          Cell(
            id = storedWorkspace.firebase.child("_append").toString, // XXX special id
            content =
              atomicContent[String](<.span("...", ^.title := "Add expression"), "").copy(menu =
                Some(MenuContent(
                  getCommands = getCommands,
                  deleteCommand = nopCommand[String]
                ))
              )
          )
        ),
        Styles.workspace
      )
    })
  }

}