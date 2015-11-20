/**
  * Sketchpad.scala - Opetopic Sketchpad Application
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.jquery._

import org.denigma.codemirror.extensions.EditorConfig
import org.denigma.codemirror.{CodeMirror, EditorConfiguration}
import org.scalajs.dom.raw.HTMLTextAreaElement

import opetopic.js.JQuerySemanticUI._

object Sketchpad extends JSApp {

  def addPane : Unit = {
    val pane = new SketchPane
    jQuery("#panes").append(pane.uiElement)
  }

  // Setup CodeMirror

  val params : EditorConfiguration = 
    EditorConfig.lineNumbers(true) 

  val codeArea = 
    document.getElementById("code-area").
      asInstanceOf[HTMLTextAreaElement]

  val editor = CodeMirror.fromTextArea(codeArea, params)

  def main : Unit = {

    println("Launched Opetopic Sketchpad.")

    jQuery(".ui.modal").modal(js.Dynamic.literal(autofocus = false))
    jQuery("#add-button").click((e : JQueryEventObject) => { addPane})
    addPane

  }


}
