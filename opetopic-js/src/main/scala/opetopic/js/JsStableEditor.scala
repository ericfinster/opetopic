/**
  * JsStableEditor.scala - Javascript Stable Editor Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.stable._
import JsDomFramework._
import JQuerySemanticUI._

class JsStableEditor[A: Renderable](c: SComplex[A]) {

  // var viewerWidth : Int = 0
  // var viewerHeight : Int = 0

  val editor = StableEditor[A, JsDomFramework.type](JsDomFramework)(c)

  val uiElement = div(tabindex := 0, style := "min-height: 200px").render

  jQuery(uiElement).append(editor.element.uiElement)

  // Install the key handler
  jQuery(uiElement).keypress((e : JQueryEventObject) => {
    e.which match {
      case 101 => editor.extrudeSelection
      case 108 => editor.loopAtSelection
      case 112 => ()
      case 120 => ()
      case _ => ()
    }
  })

}
