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
trait BaseLayout extends BaseView
    with EventSystem {
  def addView(view: View, index: Int = 0): js.Any = js.native
  def index(obj: js.Object): Int = js.native
  def reconstruct(): Unit = js.native
  def removeView(view: View): Unit = js.native
  def removeView(idx: Int): Unit = js.native
  def resizeChildren(): Unit = js.native
  def restore(state: js.Object, f: js.Function0[Unit] = { () => () }): Unit = js.native // ???
  def serialize(serializer: js.Function0[Unit]): js.Object = js.native  // ???
  def showBatch(name: String, mode: Boolean = false): Unit = js.native
}

@js.native
trait Layout extends BaseLayout {
  var cols: js.Array[View] = js.native
  var isolate: Boolean = js.native
  var margin: Int = js.native
  var on: js.Object = js.native
  var padding: Int = js.native
  var paddingX: Int = js.native
  var paddingY: Int = js.native
  var responsive: String = js.native
  var rows: js.Array[View] = js.native
  @JSName("type")
  var layoutType: String = js.native
  var visibleBatch: String = js.native
}


