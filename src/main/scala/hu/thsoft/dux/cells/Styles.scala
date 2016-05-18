package hu.thsoft.dux.cells

import scalacss.Defaults._

object Styles extends StyleSheet.Inline {
  import dsl._

  val font = fontFace("Inconsolata")(
    _.src("local(Inconsolata), url(https://fonts.gstatic.com/s/inconsolata/v12/BjAYBlHtW3CJxDcjzrnZCIgp9Q8gbYrhqGlRav_IXfk.woff2)")
  )

  val slot = style(
    position.relative,
    whiteSpace.pre,
    display.inline,
    &.hover(
      backgroundColor(c"#99ccff")
    )
  )

  val selectedSlot = style(
    boxShadow := "0 0 1px 2px #6EA3CF",
    borderRadius(2 px),
    backgroundColor(lightblue)
  )

  val selectedCellContent = style(
    borderWidth(1 px),
    borderColor.gray,
    borderStyle.solid,
    borderRadius(2 px),
    padding(0 px)
  )

  val menu = style(
    position.absolute,
    zIndex(Int.MaxValue),
    top(120 %%),
    left(0 px),
    display.block,
    padding(2 px),
    backgroundColor.lightgray,
    borderRadius(3 px)
  )

  val input = style(
    fontFamily(font),
    fontSize(100 %%)
  )

  val commandDescription = style(
     fontSize(80 %%),
     textAlign.right,
     color.lightgray
  )

  val selectedCommand = style(
    color.white,
    backgroundColor(c"#3879D9")
  )

}