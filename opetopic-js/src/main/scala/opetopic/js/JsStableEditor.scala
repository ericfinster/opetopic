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
    div(style := "min-height: 200px").render

  jQuery(uiElement).append(editor.element.uiElement)

}
