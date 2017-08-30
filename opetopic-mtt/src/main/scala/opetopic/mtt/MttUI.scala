/**
  * Mtt.scala - UI Elements for Mtt Editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mtt

import opetopic._
import opetopic.js._

import org.scalajs.jquery._

trait MttUI {

  val editor = new JsStableEditor[ExprMarker]

  def initializeUI = {
    jQuery("#article-pane").append(editor.uiElement)
    editor.initialize
  }

}
