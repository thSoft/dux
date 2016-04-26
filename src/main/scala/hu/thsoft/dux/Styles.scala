package hu.thsoft.dux

import scalacss.Defaults._

object Styles extends StyleSheet.Inline {
  import dsl._

  val expression = style(
    display.inlineBlock,
    padding(1 px),
    margin(1 px),
    &.hover(
      padding(0 px),
      borderWidth(1 px),
      borderColor.gray,
      borderStyle.solid,
      borderRadius(2 px)
    )
  )

  val functionType =
    expression

  val expressionView = style(
    display.block
  )

 val workspace = style(
    fontFamily(cells.Styles.font)
  )

}