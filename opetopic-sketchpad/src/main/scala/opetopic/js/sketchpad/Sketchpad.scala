/**
  * Sketchpad.scala - Opetopic Sketchpad Application
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import scala.scalajs.{js => sjs}
import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.jquery._

import opetopic._
import opetopic.ui._
import opetopic.js._
import JsDomFramework._
import JQuerySemanticUI._

object Sketchpad extends JSApp {

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
