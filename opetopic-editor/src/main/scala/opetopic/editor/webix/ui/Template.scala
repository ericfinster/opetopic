/**
  * Template.scala - Template Component for Webix
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.editor.webix.ui

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic.editor.webix._

@js.native
trait Template extends View
    with EventSystem
    with AtomRender
    with AtomDataLoader
    with Scrollable {
  def refresh(): Unit = js.native
  def setContent(node: js.Any): Unit = js.native
  def setHTML(html: String): Unit = js.native
  def setValues(obj: js.Object, update: Boolean = false): Unit = js.native
}

