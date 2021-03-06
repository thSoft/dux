package hu.thsoft.dux.cells

import scalacss.Defaults._

object Styles extends StyleSheet.Inline {
  import dsl._

  val font = fontFace("Inconsolata")(
    _.src("local(Inconsolata), url(https://fonts.gstatic.com/s/inconsolata/v12/BjAYBlHtW3CJxDcjzrnZCIgp9Q8gbYrhqGlRav_IXfk.woff2)")
  )

  val selectedCellClass = "selectedCell"

  val hoveredSlotClass = "hoveredSlot"

  val slot = style(
    position.relative,
    whiteSpace.pre,
    display.inline,
    unsafeExt(selector => s"$selector > .$hoveredSlotClass")(
      boxShadow := "0 0 1px 1px #6EA3CF",
      borderRadius(2 px)
    )
  )

  val selectedSlot = style(
    boxShadow := "0 0 1px 2px #6EA3CF",
    borderRadius(2 px),
    backgroundColor(lightblue)
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