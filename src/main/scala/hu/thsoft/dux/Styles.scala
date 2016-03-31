package hu.thsoft.dux

import scalacss.Defaults._

object Styles extends StyleSheet.Inline {
  import dsl._

  val cell = style(
    display.inlineBlock,
    margin(1 px),
    borderWidth(1 px),
    borderColor.gray,
    borderStyle.solid,
    borderRadius(2 px)
  )

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