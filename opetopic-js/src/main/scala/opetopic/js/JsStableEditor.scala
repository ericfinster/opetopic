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

class JsStableEditor[A: Renderable] {

  var viewerWidth : Int = 0
  var viewerHeight : Int = 0

  val editor = new StableEditor[A, JsDomFramework.type](JsDomFramework)

  val uiElement = 
    div(tabindex := 0, style := "min-height: 200px").render

  jQuery(uiElement).append(editor.element.uiElement)

  // Install the key handler
  jQuery(uiElement).keypress((e : JQueryEventObject) => {
    e.which match {
      case 101 => editor.extrudeSelection
      case 100 => ()
      case 112 => ()
      case 120 => for { _ <- editor.extend } { editor.renderAll }
      case _ => ()
    }
  })

}
