/**
  * Layout.scala - Layout Component for Webix
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
trait BaseView extends js.Object
    with Settings
    with Destruction
    with BaseBind
    with UIExtension {
  def adjust(): Unit = js.native
  def enable(): Unit = js.native
  def disable(): Unit = js.native
  def getChildViews: js.Array[View] = js.native   // Return ?
  def getFormView: View = js.native               // Return ?
  def getNode: js.Object = js.native                // Return ?
  def getParentView: js.Object = js.native          // Return ?
  def getTopParentView: View = js.native          // Return ?
  def hide(): Unit = js.native
  def isEnabled: Boolean = js.native
  def isVisible: Boolean = js.native
  def queryView(config: js.Object, mode: String = ""): js.Object = js.native // ???
  def resize(): Unit = js.native
  def show(force: Boolean = false, animation: Boolean = false): Unit = js.native

  val $width: Int = js.native
  val $height: Int = js.native
  
}

@js.native
trait View extends BaseView {
  var animate: js.Any = js.native
  var borderless: Boolean = js.native
  var container: js.Any = js.native
  var css: String = js.native
  var disabled: Boolean = js.native
  var gravity: Int = js.native
  var height: Int = js.native
  var hidden: Boolean = js.native
  var maxHeight: Int = js.native
  var maxWidth: Int = js.native
  var minHeight: Int = js.native
  var minWidth: Int = js.native
  var width: Int = js.native
}
