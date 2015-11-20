/**
  * DesignBlockPane.scala - A Pane for designing your code
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic._
import opetopic.ui._
import opetopic.js._
import opetopic.pprint._
import syntax.complex._
import syntax.cardinal._
import JsDomFramework._
import JQuerySemanticUI._

class DesignBlockPane {

  val editor = CardinalEditor[ConstString]
  // editor.onSelectAsRoot = showBoxProperties

  val paneElement =
    div(cls := "ui raised segment")(
      editor.element.uiElement
    ).render

  val uiElement = div(tabindex := 0)(
    paneElement,
    div()
  ).render

  jQuery(uiElement).keydown((e : JQueryEventObject) => {
    if (e.which == 8) {
      e.preventDefault
      // if (mode == LabelMode) deleteFromLabel
    }
  }).keypress((e : JQueryEventObject) => {
    // mode match {
    //   case DeformMode => 
        e.which match {
          case 101 => editor.extrudeSelection
          case 100 => editor.extrudeDrop
          case 112 => editor.sprout
          case _ => ()
        }
      // case LabelMode => {
      //   val c = e.which.toChar
      //   if (c.isLetterOrDigit) appendToLabel(c)
      // }
    // }
  })

}
