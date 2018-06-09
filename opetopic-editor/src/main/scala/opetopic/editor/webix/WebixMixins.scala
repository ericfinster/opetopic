/**
  * WebixMixins.scala - Mixin traits for Webix Facade
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.editor.webix

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.Dynamic.{literal => lit}

@js.native
trait ActiveContent extends js.Object {
  var activeContent: js.Object = js.native
}

@js.native
trait AtomDataLoader extends js.Object {
  def load(url: String, ltype: String = "", f: js.Function3[js.Object, js.Object, js.Object, Unit]): js.Object = js.native
  def parse(data: String, dataType: String): Unit = js.native
  def parse(data: js.Object, dataType: String): Unit = js.native
  var data: js.Array[js.Object] = js.native
  @JSName("data")
  var dataStr: String = js.native
  var dataFeed: String = js.native
  @JSName("dataFeed")
  var dataFeedFunc: Function1[String, Unit] = js.native
  var datatype: String = js.native
  var url: String = js.native
}

@js.native
trait AtomRender extends js.Object {
  def render(id: String, data: js.Object, rtype: String): Unit = js.native
  def sync(source: js.Object, filter: Function1[js.Object, Boolean], silent: Boolean): Unit = js.native
  var template: String = js.native
  @JSName("template")
  var templateFunc: Function1[js.Object, String] = js.native
}

@js.native
trait AutoScroll extends js.Object {
  var dragscroll: Boolean = js.native
}

@js.native
trait AutoTooltip extends js.Object {
  var tooltip: String = js.native
  @JSName("tooltip")
  var tooltipBool: Boolean = js.native
}

@js.native
trait BaseBind extends js.Object {
  def bind(target: js.Object): Unit = js.native  // Incomplete
  def unbind(): Unit = js.native
}

@js.native
trait Destruction extends js.Object {
  def destructor(): Unit = js.native
}

@js.native
trait EventSystem extends js.Object {
  def attachEvent(typ: String, f: js.Function0[Unit], id: String = ""): String = js.native
  def blockEvent(): Unit = js.native
  def callEvent(name: String, params: js.Array[js.Any]): Boolean = js.native
  def detachEvent(id: String): Unit = js.native
  def hasEvent(name: String): Boolean = js.native
  def mapEvent(map: js.Object): Unit = js.native
  def unblockEvent(): Unit = js.native
}

@js.native
trait MouseEvents extends js.Object {
  var mouseEventDelay: Int = js.native
  var onClick: js.Object = js.native
  var onContext: js.Object = js.native
  var onDblClick: js.Function2[js.Object, js.Object, Unit] = js.native     // ???
  var onMouseMove: js.Function2[js.Object, js.Object, Unit] = js.native    // ???
}

@js.native
trait Scrollable extends js.Object {
  def getScrollState: js.Object = js.native
  def scrollTo(x: Int, y: Int): Unit = js.native
  var scroll: String = js.native
  @JSName("scroll")
  var scrollBool: Boolean = js.native
  var scrollSpeed: String = js.native
}

@js.native
trait Settings extends js.Object {
  def define(str: String, value: js.Object): Unit = js.native
  var id: String = js.native
  val name: String = js.native
  val config: js.Object = js.native
}

@js.native
trait UIExtension extends js.Object




