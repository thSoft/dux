package hu.thsoft.dux.cells

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

class Render[CellId](editorState: Option[EditorState[CellId]], editorStateObserver: Observer[Option[EditorState[CellId]]]) {

  def selected(it: Cell[CellId]) = {
    editorState.map(_.selection.cellId == it.id).getOrElse(false)
  }

  def cell(it: Cell[CellId]): ReactElement = {
    val leftMenu = menuContainer(it, LeftMenu, EmptyTag)
    val rightMenu = menuContainer(it, RightMenu, EmptyTag)
    val content: ReactElement =
      <.span(
        it.content match {
          case content: AtomicContent[CellId] =>
            menuContainer(it, ContentMenu, content.element)
          case content: CompositeContent[CellId] =>
            <.span(
              content.children.map(cell(_)),
              content.tagMod
            )
        },
        selected(it) ?= (^.backgroundColor := "lightblue")
      )
    <.span(
      leftMenu,
      content,
      rightMenu
    )
  }

  def menuContainer(cell: Cell[CellId], menuId: MenuId, element: TagMod): ReactElement = {
    val theMenu =
      if (selected(cell)) {
        editorState.flatMap(editorState => {
          if (editorState.selection.menuId == menuId) {
            val selectedMenu: Menu =
                menuId match {
                case LeftMenu => cell.content.leftMenu
                case RightMenu => cell.content.rightMenu
                case ContentMenu =>
                cell.content match {
                case content: AtomicContent[CellId] => content.menu
                case content: CompositeContent[CellId] => None
                }
          }
          val menuCommands = selectedMenu.map(getCommands => getCommands(editorState.input))
              menuCommands.map(commands => {
                menu(editorState, commands)
              })
          } else {
            None
          }
        })
      } else None
    <.span(
      element,
      ^.onClick ==> ((event: SyntheticMouseEvent[Node]) => Callback {
        event.stopPropagation()
        val selection = Selection(cell.id, menuId)
        val initialInput =
          cell.content match {
          case content: AtomicContent[CellId] => content.stringValue
          case _ => ""
          }
        editorStateObserver.onNext(Some(EditorState(selection, initialInput, 0)))
      }),
      ^.position.relative,
      theMenu
    )
  }

  def menu(editorState: EditorState[CellId], commands: List[Command]): ReactElement =
    <.div(
      Styles.editor,
      <.input(
        ^.autoFocus := true,
        ^.value := editorState.input,
        ^.defaultValue := "",
        ^.onInput ==> ((event: SyntheticCompositionEvent[HTMLInputElement]) => Callback {
          editorStateObserver.onNext(Some(editorState.copy(input = event.target.value)))
        }),
        ^.onKeyDown ==>? ((event: SyntheticKeyboardEvent[Node]) => {
          def moveCommandIndex(delta: Int) =
            Some(Callback {
              event.preventDefault()
              editorStateObserver.onNext(Some(editorState.copy(selectedCommandIndex = Math.floorMod(editorState.selectedCommandIndex + delta, commands.size))))
            })
          event.key match {
            case KeyValue.ArrowDown => moveCommandIndex(1)
            case KeyValue.ArrowUp => moveCommandIndex(-1)
            case KeyValue.ArrowLeft => None // TODO navigate to previous if at start
            case KeyValue.ArrowRight => None // TODO navigate to next if at end
            case KeyValue.Enter =>
              commands.lift(editorState.selectedCommandIndex).map(_.callback.thenRun(
                editorStateObserver.onNext(None)
              ).void)
            case _ => None
          }
        })
      ),
      <.div(
        commands.zipWithIndex.map { case (command, index) =>
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

}