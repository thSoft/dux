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
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import org.scalajs.dom.ext.KeyValue
import org.scalajs.dom
import scalaz.syntax.std.stream._
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.Console
import monix.reactive.Observer
import org.scalajs.dom.raw.HTMLSpanElement

object Render {

  def apply[CellId](root: Cell[CellId], editorState: Option[EditorState[CellId]], editorStateObserver: Observer[Option[EditorState[CellId]]]): ReactElement = {

    case class Slot[CellId](
      id: SlotId[CellId],
      initialInput: String
    )

    lazy val slotZipper = makeSlotList(root).toStream.toZipper

    def makeSlotList(cell: Cell[CellId]): List[Slot[CellId]] = {
      def slotList(menu: Menu[CellId], slotType: SlotType) = {
        val initialInput = getInitialInput(cell, slotType)
        if (menu.isDefined)
          List(
            Slot(
              id = SlotId(cellId = cell.id, slotType = slotType),
              initialInput = initialInput
            )
          )
        else List()
      }
      val contentSlotList =
        cell.content match {
          case content: AtomicContent[CellId] => slotList(content.menu, ContentSlot)
          case content: CompositeContent[CellId] => content.children.flatMap(child => makeSlotList(child))
        }
      val leftSlotList = slotList(cell.content.leftMenu, LeftSlot)
      val rightSlotList = slotList(cell.content.rightMenu, RightSlot)
      leftSlotList ++ contentSlotList ++ rightSlotList
    }

    def getInitialInput(cell: Cell[CellId], slotType: SlotType): String = {
      slotType match {
        case LeftSlot => ""
        case ContentSlot => {
          cell.content match {
            case content: AtomicContent[CellId] => content.stringValue
            case content: CompositeContent[CellId] => ""
          }
        }
        case RightSlot => ""
      }
    }

    def cellSelected(cell: Cell[CellId]): Boolean = {
      editorState.map(_.selection.cellId == cell.id).getOrElse(false)
    }

    def slotSelected(slotId: SlotId[CellId]): Boolean = {
      editorState.map(_.selection == slotId).getOrElse(false)
    }

    def renderCell(cell: Cell[CellId]): ReactElement = {
      val leftSlot = renderSlot(cell, LeftSlot, " ")
      val rightSlot = renderSlot(cell, RightSlot, " ")
      val contentSlot =
        <.span(
          cell.content match {
            case content: AtomicContent[CellId] =>
              renderSlot(cell, ContentSlot, content.element)
            case content: CompositeContent[CellId] =>
              <.span(
                content.children.map(renderCell(_)),
                content.tagMod
              )
          },
          cellSelected(cell) ?= Styles.selectedCell
        )
      <.span(
        leftSlot,
        contentSlot,
        rightSlot
      )
    }

    def renderSlot(cell: Cell[CellId], slotType: SlotType, tagMod: TagMod): Option[ReactElement] = {
      val menu =
        slotType match {
          case LeftSlot => cell.content.leftMenu
          case RightSlot => cell.content.rightMenu
          case ContentSlot =>
            cell.content match {
              case content: AtomicContent[CellId] => content.menu
              case content: CompositeContent[CellId] => None
            }
        }
      menu.map(menuContent => {
        val slotId = SlotId(cellId = cell.id, slotType = slotType)
        val slot =
          Slot(
            id = slotId,
            initialInput = getInitialInput(cell, slotType)
          )
        val onClick =
          ^.onClick ==> ((event: SyntheticMouseEvent[HTMLElement]) => Callback {
            event.stopPropagation()
            val newEditorState = makeEditorState(slot, dom.window.getSelection().focusOffset)
            editorStateObserver.onNext(Some(newEditorState))
          })
        val selected = slotSelected(slotId)
        val selectedMenu =
          if (selected) {
            editorState.flatMap(editorState => {
              menu.map(menuContent => {
                renderMenu(cell, slotType, editorState, menuContent)
              })
            })
          } else None
        <.span(
          tagMod,
          onClick,
          selectedMenu,
          Styles.slot,
          selected ?= Styles.selectedSlot
        )
      })
    }

    def renderMenu(cell: Cell[CellId], slotType: SlotType, editorState: EditorState[CellId], menuContent: MenuContent[CellId]): ReactElement = {
      val commands = menuContent.getCommands(editorState.input)
      val inputView =
        <.input(
          Styles.input,
          ^.autoFocus := true,
          ^.value := editorState.input,
          ^.onFocus ==> ((event: SyntheticFocusEvent[HTMLInputElement]) => Callback {
            event.target.selectionStart = editorState.inputCaretIndex
          }),
          ^.onChange ==> ((event: SyntheticCompositionEvent[HTMLInputElement]) => Callback {
            editorStateObserver.onNext(Some(editorState.copy(input = event.target.value)))
          }),
          ^.onKeyDown ==> ((event: SyntheticKeyboardEvent[HTMLInputElement]) => {
            def moveCursor(right: Boolean): Callback =
              Callback {
                val atStart = event.target.selectionStart == 0
                val atEnd = event.target.selectionEnd == editorState.input.length
                if ((atStart && !right) || (atEnd && right)) {
                  doNavigate(right)
                }
              }
            def doNavigate(right: Boolean) = {
              val newSlot = navigate(editorState.selection, right)
              val newEditorState = newSlot.map(slot => {
                val caretIndex = if (!right) slot.initialInput.length else 0
                makeEditorState(slot, caretIndex)
              })
              editorStateObserver.onNext(newEditorState)
              event.preventDefault()
            }
            val navigator = new Navigator[CellId] {
              def navigateTo(slotId: SlotId[CellId]): Unit = {
                val nextEditorState =
                  EditorState(
                    selection = slotId,
                    input = "",
                    inputCaretIndex = 0,
                    selectedCommandIndex = 0
                  )
                editorStateObserver.onNext(Some(nextEditorState))
                ()
              }
              def navigateLeft = doNavigate(false)
              def navigateRight = doNavigate(true)
            }
            def moveCommandIndex(delta: Int): Callback =
              Callback {
                event.preventDefault()
                editorStateObserver.onNext(Some(editorState.copy(selectedCommandIndex = Math.floorMod(editorState.selectedCommandIndex + delta, commands.size))))
              }
            def handleDelete(slotTypeToCheck: SlotType): Callback = {
              Callback.ifTrue(
                (slotTypeToCheck == slotType) && (editorState.input == ""),
                menuContent.deleteCommand.callback(navigator).thenRun {
                  event.preventDefault()
                  ()
                }
              )
            }
            event.key match {
              case KeyValue.ArrowDown =>
                moveCommandIndex(1)
              case KeyValue.ArrowUp =>
                moveCommandIndex(-1)
              case KeyValue.ArrowLeft =>
                moveCursor(false)
              case KeyValue.ArrowRight =>
                moveCursor(true)
              case KeyValue.Tab =>
                Callback { doNavigate(!event.shiftKey) }
              case KeyValue.Enter | KeyValue.Spacebar =>
                commands.lift(editorState.selectedCommandIndex).map(command =>
                  command.callback(navigator)
                ).getOrElse(Callback.empty)
              case KeyValue.Backspace =>
                handleDelete(RightSlot)
              case KeyValue.Delete =>
                handleDelete(LeftSlot)
              case _ => Callback.empty
            }
          })
        )
      val commandsView =
        <.div(
          commands.zipWithIndex.map { case (command, index) =>
            <.div(
              (index == editorState.selectedCommandIndex) ?= Styles.selectedCommand,
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
      <.div(
        Styles.menu,
        inputView,
        commandsView
      )
    }

    def navigate(selection: SlotId[CellId], right: Boolean): Option[Slot[CellId]] = {
      slotZipper.flatMap(slotZipper =>
        slotZipper.findZ(_.id == selection).map(if (right) _.nextC else _.previousC).map(_.focus)
      )
    }

    def makeEditorState(slot: Slot[CellId], inputCaretIndex: Int): EditorState[CellId] = {
      EditorState(
        selection = slot.id,
        input = slot.initialInput,
        inputCaretIndex = inputCaretIndex,
        selectedCommandIndex = 0
      )
    }

    renderCell(root)
  }

}
