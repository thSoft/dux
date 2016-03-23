package hu.thsoft.dux

import scala.scalajs.js
import scala.scalajs.js.JSApp

import org.scalajs.dom.document

import hu.thsoft.firebase.Firebase
import japgolly.scalajs.react.ReactDOM
import monifu.concurrent.Implicits.globalScheduler

object Main extends JSApp {

  def main(): Unit = {
    val mapping = mappings.expression
    val model = mapping.observe(new Firebase("https://thsoft.firebaseio.com/DUX/test/Expression"))
    val view = model.map(views.expression)

    val container = document.createElement("div")
    document.body.appendChild(container)
    view.foreach(ReactDOM.render(_, container))
  }

}