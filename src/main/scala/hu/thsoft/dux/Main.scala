package hu.thsoft.dux

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom.document
import hu.thsoft.firebase.Firebase
import japgolly.scalajs.react.ReactDOM
import monifu.concurrent.Implicits.globalScheduler
import hu.thsoft.firebasemodel.Mapping
import japgolly.scalajs.react.vdom.prefix_<^._
import monifu.reactive.OverflowStrategy
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import monifu.reactive.subjects.PublishSubject
import monifu.reactive.subjects.BehaviorSubject
import hu.thsoft.dux.cells.EditorState
import hu.thsoft.dux.cells.Render

object Main extends JSApp {

  def main(): Unit = {
    val mapping = mappings.workspace
    val model = mapping.observe(new Firebase("https://thsoft.firebaseio.com/DUX/test/Workspace"))
    val editorState = BehaviorSubject[Option[EditorState[String]]](None)
    val view = model.combineLatest(editorState).map { case (currentModel, currentEditorState) =>
      val cell = Cells.workspace(currentModel)
      Render(cell, currentEditorState, editorState)
    }

    List(cells.Styles, Styles).foreach(_.addToDocument())
    val container = document.createElement("div")
    document.body.appendChild(container)
    view.foreach(element => ReactDOM.render(element, container))
  }

}