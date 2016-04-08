package hu.thsoft.dux.cells

import org.scalajs.dom.raw.HTMLInputElement
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
import org.scalajs.dom
import scalaz.syntax.std.stream._
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.Console

object Render {

  def apply[CellId](root: Cell[CellId], editorState: Option[EditorState[CellId]], editorStateObserver: Observer[Option[EditorState[CellId]]]): ReactElement = {

    case class MenuContainer[CellId](
      initialInput: String,
      selection: Selection[CellId]
    )

    lazy val menuContainerZipper = makeMenuContainerList(root).toStream.toZipper

    def makeMenuContainerList(cell: Cell[CellId]): List[MenuContainer[CellId]] = {
      def menuContainerList(menu: Menu, menuId: MenuId) = {
        val initialInput = getInitialInput(cell, menuId)
        if (menu.isDefined) List(MenuContainer(initialInput, Selection(cell.id, menuId))) else List()
      }
      val contentMenus =
        cell.content match {
          case content: AtomicContent[CellId] => menuContainerList(content.menu, ContentMenu)
          case content: CompositeContent[CellId] => content.children.flatMap(child => makeMenuContainerList(child))
        }
      val leftMenus = menuContainerList(cell.content.leftMenu, LeftMenu)
      val rightMenus = menuContainerList(cell.content.rightMenu, RightMenu)
      leftMenus ++ contentMenus ++ rightMenus
    }

    def getInitialInput(cell: Cell[CellId], menuId: MenuId): String = {
      menuId match {
        case LeftMenu => ""
        case ContentMenu => {
          cell.content match {
            case content: AtomicContent[CellId] => content.stringValue
            case content: CompositeContent[CellId] => ""
          }
        }
        case RightMenu => ""
      }
    }

    def selected(it: Cell[CellId]): Boolean = {
      editorState.map(_.selection.cellId == it.id).getOrElse(false)
    }

    def sideMenuElement(menu: Menu): TagMod = {
      if (menu.isDefined) " " else EmptyTag
    }

    def cell(it: Cell[CellId]): ReactElement = {
      val leftMenu = menuContainer(it, LeftMenu, sideMenuElement(it.content.leftMenu))
      val rightMenu = menuContainer(it, RightMenu, sideMenuElement(it.content.rightMenu))
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
                menu(cell, editorState, commands)
              })
            } else {
              None
            }
          })
        } else None
      <.span(
        element,
        ^.onClick ==> ((event: SyntheticMouseEvent[HTMLElement]) => Callback {
          event.stopPropagation()
          val menuContainer = MenuContainer(getInitialInput(cell, menuId), Selection(cell.id, menuId))
          val newEditorState = makeEditorState(menuContainer, dom.window.getSelection().focusOffset)
          editorStateObserver.onNext(Some(newEditorState))
        }),
        ^.position.relative,
        theMenu
      )
    }

    def menu(cell: Cell[CellId], editorState: EditorState[CellId], commands: List[Command]): ReactElement =
      <.div(
        Styles.editor,
        <.input(
          ^.autoFocus := true,
          ^.value := editorState.input,
          ^.onFocus ==> ((event: SyntheticFocusEvent[HTMLInputElement]) => Callback {
            event.target.selectionStart = editorState.inputCaretIndex
          }),
          ^.onInput ==> ((event: SyntheticCompositionEvent[HTMLInputElement]) => Callback {
            editorStateObserver.onNext(Some(editorState.copy(input = event.target.value)))
          }),
          ^.onKeyDown ==> ((event: SyntheticKeyboardEvent[HTMLInputElement]) => {
            def moveCommandIndex(delta: Int) =
              Callback {
                event.preventDefault()
                editorStateObserver.onNext(Some(editorState.copy(selectedCommandIndex = Math.floorMod(editorState.selectedCommandIndex + delta, commands.size))))
              }
            def doNavigate(right: Boolean): Callback =
              Callback {
                val atStart = event.target.selectionStart == 0
                val atEnd = event.target.selectionEnd == editorState.input.length
                if ((atStart && !right) || (atEnd && right)) {
                  val menuContainerOption = navigate(editorState.selection, right)
                  val newEditorState = menuContainerOption.map(menuContainer => {
                    val caretIndex = if (!right) menuContainer.initialInput.length else 0
                    makeEditorState(menuContainer, caretIndex)
                  })
                  editorStateObserver.onNext(newEditorState)
                  event.preventDefault()
                }
              }
            event.key match {
              case KeyValue.ArrowDown =>
                moveCommandIndex(1)
              case KeyValue.ArrowUp =>
                moveCommandIndex(-1)
              case KeyValue.ArrowLeft =>
                doNavigate(false)
              case KeyValue.ArrowRight =>
                doNavigate(true)
              case KeyValue.Enter =>
                commands.lift(editorState.selectedCommandIndex).map(_.callback >> doNavigate(true)).getOrElse(Callback.empty)
              case _ => Callback.empty
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
                command.description,
                Styles.commandDescription
              )
            )
          }
        )
      )

    def navigate(selection: Selection[CellId], right: Boolean): Option[MenuContainer[CellId]] = {
      menuContainerZipper.flatMap(menuContainerZipper =>
        menuContainerZipper.findZ(_.selection == selection).map(if (right) _.nextC else _.previousC).map(_.focus)
      )
    }

    def makeEditorState(menuContainer: MenuContainer[CellId], inputCaretIndex: Int): EditorState[CellId] = {
      EditorState(menuContainer.selection, menuContainer.initialInput, inputCaretIndex, 0)
    }

    cell(root)
  }

}
