/**
  * JsEditor.scala - A Cardinal Editor in JavaScript
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs.{js => sjs}
import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.jquery._

import opetopic._
import opetopic.ui._
import syntax.complex._
import syntax.cardinal._
import JsDomFramework._
import JQuerySemanticUI._

object JsEditor extends JSApp {

  def addPane : Unit = {
    val pane = new EditorPane
    jQuery("#panes").append(pane.uiElement)
  }

  def main : Unit = {

    println("Launched Opetopic.")

    jQuery("#add-button").click((e : JQueryEventObject) => { addPane})
    addPane

  }


}
