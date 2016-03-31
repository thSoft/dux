package hu.thsoft.dux

import japgolly.scalajs.react.Callback

object Editor {

  case class State[Selection](
    selection: Selection,
    input: String,
    selectedCommandIndex: Int
  )

  case class Props(
    commands: List[Command]
  )

  case class Command(
    text: String,
    description: String,
    callback: Callback
  )

}
