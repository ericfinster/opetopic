/**
  * Paper.scala - Paper Component for Webix
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.editor.webix.ui

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.Dynamic.{literal => lit}

import org.scalajs.dom
import org.scalajs.dom.html

import opetopic.editor.webix._

@js.native
trait Paper extends View {
  val canvas: html.Canvas = js.native
  var isSetup: Boolean = js.native
}
