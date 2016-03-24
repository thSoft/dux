package hu.thsoft.dux

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom.document
import hu.thsoft.firebase.Firebase
import japgolly.scalajs.react.ReactDOM
import monifu.concurrent.Implicits.globalScheduler
import hu.thsoft.firebasemodel.Mapping
import japgolly.scalajs.react.vdom.prefix_<^._

object Main extends JSApp {

  def main(): Unit = {
    val mapping = mappings.workspace
    val model = mapping.observe(new Firebase("https://thsoft.firebaseio.com/DUX/test/Workspace"))
    val view = model.map(views.workspace(_))

    val container = document.createElement("div")
    document.body.appendChild(container)
    view.foreach(ReactDOM.render(_, container))
  }

}