package hu.thsoft.dux

import scalacss.Defaults._

object Styles extends StyleSheet.Inline {
  import dsl._

  val hoverClass = "hoveredExpression"

  val selectedBackgroundColor = backgroundColor(c"#f0f0ff")

  val expression = style(
    display.inline,
    padding(2 px),
    margin(1 px),
    unsafeRoot(s".$hoverClass")(
      padding(1 px),
      borderWidth(1 px),
      borderColor.lightgray,
      borderStyle.solid,
      borderRadius(2 px),
      backgroundColor(c"#eeeeff")
    ),
    unsafeExt(selector => s"$selector.${cells.Styles.selectedCellClass}")(
      boxShadow := "0 0 1px 1px #dddddd",
      borderRadius(2 px),
      selectedBackgroundColor
    )
  )

  val functionType =
    expression

  val expressionView = style(
    display.inlineBlock,
    padding(2 px),
    margin(2 px),
    lineHeight(150 %%),
    borderWidth(1 px),
    borderColor.black,
    borderStyle.solid,
    borderRadius(3 px),
    boxShadow := "1px 1px 1px 1px lightgray",
    unsafeExt(selector => s".${cells.Styles.selectedCellClass} $selector")(
      selectedBackgroundColor
    )
  )

  val workspace = style(
    fontFamily(cells.Styles.font)
  )

}