/**
  * Webix.scala - Webix Facade for ScalaJs
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.editor.webix

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.Dynamic.{literal => lit}

import ui.View

@js.native
@JSGlobal("webix")
object Webix extends js.Object {
  def ready(f: js.Function0[Unit]): Unit = js.native
  val ui: WebixUIHelper = js.native
  def ui(cfg: js.Object): View = js.native
  def ui(cfg: js.Object, parent: View): View = js.native
  def protoUI(tgt: js.Object, mixins: js.Object*): View = js.native
  val UIManager: UIManager = js.native
}

@js.native
@JSGlobal("$$")
object WebixView extends js.Object {
  def apply(id: String): View = js.native
}

@js.native
trait WebixUIHelper extends js.Object {

  def fullScreen(): Unit = js.native
  def zIndex(): Int = js.native

}

