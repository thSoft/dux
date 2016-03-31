package hu.thsoft.dux

import japgolly.scalajs.react.vdom.prefix_<^._
import hu.thsoft.dux.types._
import japgolly.scalajs.react.ReactElement
import hu.thsoft.firebasemodel.Mapping._
import monifu.reactive.Channel
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ReactEventAliases
import japgolly.scalajs.react.SyntheticEvent
import org.scalajs.dom.raw.Node
import japgolly.scalajs.react.SyntheticCompositionEvent
import japgolly.scalajs.react.SyntheticKeyboardEvent
import org.scalajs.dom.ext.KeyValue
import scalacss.ScalaCssReact._
import monifu.reactive.Observer
import japgolly.scalajs.react.SyntheticMouseEvent
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLInputElement

class Views(editorState: Option[Editor.State[String]], editorStateObserver: Observer[Option[Editor.State[String]]]) {

  def stored[T](stored: Stored[T])(view: T => ReactElement): ReactElement = {
    val selected = stored.firebase.toString == editorState.map(_.selection).getOrElse(false)
    <.span(
      stored.value match {
        case Left(invalid) =>
          <.a(
            "⚠",
            ^.href := stored.firebase.toString,
            ^.target := "_blank",
            ^.title := s"Expected ${invalid.expectedTypeName} but got ${invalid.json}"
          )
        case Right(value) => view(value)
      },
      ^.onClick ==> ((event: SyntheticMouseEvent[Node]) => Callback {
        event.stopPropagation()
        editorStateObserver.onNext(Some(Editor.State(stored.firebase.toString, "", 0)))
      }),
      selected ?=
        editor(Editor.Props(
          List(
            Editor.Command("Foo", "Bar", Callback.alert("foo")),
            Editor.Command("Qux", "Baz", Callback {})
          )
        )),
      selected ?= (^.backgroundColor := "lightblue"),
      ^.position.relative
    )
  }

  def double(double: Stored[Double]): ReactElement =
    stored(double)(value => <.span(value))

  def functionType(functionType: FunctionType): ReactElement =
    <.div(
      functionType match {
        case Add => "+"
        case Subtract => "-"
        case Multiply => "*"
        case Divide => "*"
      },
      Styles.cell
    )

  def expression(expression: Expression): ReactElement =
    expression match {
      case numberLiteralExpression: NumberLiteral => numberLiteral(numberLiteralExpression)
      case functionCallExpression: FunctionCall => functionCall(functionCallExpression)
    }

  def numberLiteral(numberLiteral: NumberLiteral): ReactElement =
    <.div(
      double(numberLiteral.value),
      Styles.cell
    )

  def functionCall(functionCall: FunctionCall): ReactElement =
    <.div(
      stored(functionCall.firstArgument)(expression),
      stored(functionCall.functionType)(functionType),
      stored(functionCall.secondArgument)(expression),
      Styles.cell
    )

  def expressionView(expressionView: ExpressionView): ReactElement =
    <.div(
      stored(expressionView.expression)(storedExpression =>
        stored(storedExpression)(expression)
      )
    )

  def workspace(workspace: Workspace): ReactElement =
    <.div(
      stored(workspace.views)(views =>
        <.div(
          views.map(view =>
            stored(view)(expressionView)
          )
        )
      )
    )

  def editor(props: Editor.Props): Option[ReactElement] =
    editorState.map(editorState =>
      <.div(
        Styles.editor,
        <.input(
          ^.autoFocus := true,
          ^.value := editorState.input,
          ^.onInput ==> ((event: SyntheticCompositionEvent[HTMLInputElement]) => Callback {
            editorStateObserver.onNext(Some(editorState.copy(input = event.target.value)))
          }),
          ^.onKeyDown ==>? ((event: SyntheticKeyboardEvent[Node]) => {
            def moveCommandIndex(delta: Int) =
              Some(Callback {
                event.preventDefault()
                editorStateObserver.onNext(Some(editorState.copy(selectedCommandIndex = Math.floorMod(editorState.selectedCommandIndex + delta, props.commands.size))))
              })
            event.key match {
              case KeyValue.ArrowDown => moveCommandIndex(1)
              case KeyValue.ArrowUp => moveCommandIndex(-1)
              case KeyValue.Enter => props.commands.lift(editorState.selectedCommandIndex).map(_.callback)
              case _ => None
            }
          })
        ),
        <.div(
          props.commands.zipWithIndex.map { case (command, index) =>
            <.div(
              index == editorState.selectedCommandIndex ?= Styles.selectedCommand,
              <.div(
                command.text
              ),
              <.div(
                command.description
              )
            )
          }
        )
      )
    )

}