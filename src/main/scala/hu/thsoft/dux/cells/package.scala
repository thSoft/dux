package hu.thsoft.dux

import japgolly.scalajs.react.ReactElement
import scala.concurrent.Future
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.prefix_<^._

package object cells {

  type Menu =
    Option[String => List[Command]]

  case class Cell[CellId](
    id: CellId,
    content: CellContent[CellId]
  )

  sealed trait CellContent[CellId] {
    def leftMenu: Menu
    def rightMenu: Menu
  }
  case class AtomicContent[CellId](
    element: ReactElement,
    stringValue: String,
    menu: Menu,
    leftMenu: Menu,
    rightMenu: Menu
  ) extends CellContent[CellId]
  case class CompositeContent[CellId](
    children: List[Cell[CellId]],
    tagMod: TagMod,
    leftMenu: Menu,
    rightMenu: Menu
  ) extends CellContent[CellId]

  def atomicContent[CellId](element: ReactElement, stringValue: String): AtomicContent[CellId] = {
    AtomicContent(element, stringValue, None, None, None)
  }

  def compositeContent[CellId](children: Cell[CellId]*): CompositeContent[CellId] = {
    CompositeContent(children.toList, EmptyTag, None, None)
  }

  def compositeContent[CellId](children: List[Cell[CellId]], tagMod: TagMod = EmptyTag): CompositeContent[CellId] = {
    CompositeContent(children, tagMod, None, None)
  }

  case class EditorState[CellId](
    selection: SlotId[CellId],
    input: String,
    inputCaretIndex: Int,
    selectedCommandIndex: Int
  )

  case class SlotId[CellId](
    cellId: CellId,
    slotType: SlotType
  )

  sealed trait SlotType
  case object LeftSlot extends SlotType
  case object ContentSlot extends SlotType
  case object RightSlot extends SlotType

  case class Command(
    text: String,
    description: String,
    callback: Callback
  )

}
