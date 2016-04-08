package hu.thsoft.dux.cells

import scalacss.Defaults._

object Styles extends StyleSheet.Inline {
  import dsl._

  val menu = style(
    position.absolute,
    zIndex(Int.MaxValue),
    top(100 %%),
    left(0 px),
    display.block,
    backgroundColor.lightgray
  )

  val selected = style(
    backgroundColor.lightblue
  )

  val slot = style(
    position.relative
  )

  val selectedCommand = style(
    color.white,
    backgroundColor.blue
  )

  val commandDescription = style(
     fontSize(80 %%),
     textAlign.right,
     color.gray
  )

}