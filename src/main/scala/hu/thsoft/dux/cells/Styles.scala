package hu.thsoft.dux.cells

import scalacss.Defaults._

object Styles extends StyleSheet.Inline {
  import dsl._

  val editor = style(
    position.absolute,
    zIndex(Int.MaxValue),
    top(100 %%),
    left(0 px),
    display.block,
    backgroundColor.lightgray
  )

  val selectedCommand = style(
    color.white,
    backgroundColor.blue
  )

}