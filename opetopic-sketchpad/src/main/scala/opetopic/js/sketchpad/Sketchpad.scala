/**
  * Sketchpad.scala - Opetopic Sketchpad Application
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import scala.scalajs.js.JSApp
import org.scalajs.jquery._

object Sketchpad extends JSApp {

  def addPane : Unit = {
    val pane = new SketchPane
    jQuery("#panes").append(pane.uiElement)
  }

  def main : Unit = {

    println("Launched Opetopic Sketchpad.")

    jQuery("#add-button").click((e : JQueryEventObject) => { addPane})
    addPane

  }


}
