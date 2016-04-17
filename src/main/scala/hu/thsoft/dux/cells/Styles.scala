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
    padding(2 px),
    backgroundColor.lightgray,
    borderRadius(3 px)
  )

  val selected = style(
    backgroundColor(c"#B0D8FB"),
    boxShadow := "0 0 1px 2px #6EA3CF"
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