package hu.thsoft.dux

import japgolly.scalajs.react.ReactElement
import scala.concurrent.Future
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.prefix_<^._

package object cells {

  case class Cell[CellId](
    id: CellId,
    content: CellContent[CellId]
  )

  def setLeftMenu[CellId](cell: Cell[CellId], leftMenu: Menu[CellId]): Cell[CellId] = {
    val newContent: CellContent[CellId] =
      cell.content match {
        case content: AtomicContent[CellId] => content.copy(leftMenu = leftMenu)
        case content: CompositeContent[CellId] => content.copy(leftMenu = leftMenu)
      }
    cell.copy(content = newContent)
  }

  def setRightMenu[CellId](cell: Cell[CellId], rightMenu: Menu[CellId]): Cell[CellId] = {
    val newContent: CellContent[CellId] =
      cell.content match {
        case content: AtomicContent[CellId] => content.copy(rightMenu = rightMenu)
        case content: CompositeContent[CellId] => content.copy(rightMenu = rightMenu)
      }
    cell.copy(content = newContent)
  }

  sealed trait CellContent[CellId] {
    def leftMenu: Menu[CellId]
    def rightMenu: Menu[CellId]
  }
  case class AtomicContent[CellId](
    element: ReactElement,
    stringValue: String,
    menu: Menu[CellId],
    leftMenu: Menu[CellId],
    rightMenu: Menu[CellId]
  ) extends CellContent[CellId]
  case class CompositeContent[CellId](
    children: List[Cell[CellId]],
    tagMod: TagMod,
    leftMenu: Menu[CellId],
    rightMenu: Menu[CellId]
  ) extends CellContent[CellId]

  def atomicContent[CellId](element: ReactElement, stringValue: String): AtomicContent[CellId] = {
    AtomicContent(
      element = element,
      stringValue = stringValue,
      menu = None,
      leftMenu = None,
      rightMenu = None
    )
  }

  def compositeContent[CellId](children: Cell[CellId]*): CompositeContent[CellId] = {
    CompositeContent(
      children = children.toList,
      tagMod = EmptyTag,
      leftMenu = None,
      rightMenu = None
    )
  }

  def compositeContent[CellId](children: List[Cell[CellId]], tagMod: TagMod = EmptyTag): CompositeContent[CellId] = {
    CompositeContent(
      children = children,
      tagMod = tagMod,
      leftMenu = None,
      rightMenu = None
    )
  }

  type Menu[CellId] =
    Option[MenuContent[CellId]]

  case class MenuContent[CellId](
    getCommands: String => List[Command[CellId]],
    deleteCommand: Command[CellId]
  )

  case class Command[CellId](
    text: String,
    description: String,
    callback: Callback,
    navigation: Navigation
  )

  def command[CellId](callback: Callback, navigation: Navigation): Command[CellId] = {
    Command("", "", callback, navigation)
  }

  sealed trait Navigation
  case object NoNavigation extends Navigation
  case class NavigateTo[CellId](slotId: SlotId[CellId]) extends Navigation
  case object NavigateRight extends Navigation
  case object NavigateLeft extends Navigation

  case class SlotId[CellId](
    cellId: CellId,
    slotType: SlotType
  )

  sealed trait SlotType
  case object LeftSlot extends SlotType
  case object ContentSlot extends SlotType
  case object RightSlot extends SlotType

  case class EditorState[CellId](
    selection: SlotId[CellId],
    input: String,
    inputCaretIndex: Int,
    selectedCommandIndex: Int
  )

}
