package hu.thsoft.dux

import scalacss.Defaults._

object Styles extends StyleSheet.Inline {
  import dsl._

  val font = fontFace("Inconsolata")(
    _.src("url(https://fonts.gstatic.com/s/inconsolata/v12/BjAYBlHtW3CJxDcjzrnZCIgp9Q8gbYrhqGlRav_IXfk.woff2)")
  )

  val expression = style(
    display.inlineBlock,
    padding(1 px),
    margin(1 px),
    borderWidth(1 px),
    borderColor.gray,
    borderStyle.solid,
    borderRadius(2 px)
  )

  val functionType =
    expression

  val expressionView = style(
    display.block
  )

 val workspace = style(
    fontFamily(font)
  )

}