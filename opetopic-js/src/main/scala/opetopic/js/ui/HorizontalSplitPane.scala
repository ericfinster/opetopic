/**
  * HorizontalSplitPane.scala - A horizontal split pane
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.ui

import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js
import org.scalajs.dom.window.document

@js.native
trait Offset extends js.Any {
  val top: Int = js.native
  val left: Int = js.native
}

class HorizontalSplitPane(val left: Component, val right: Component, initFactor: Double = 0.5) extends Component {

  val divider = div(id := "divider",
    style := "min-width: 5px; background: #36383a; cursor: ew-resize; user-drag: none;"
  ).render

  val uiElement =
    div(style := "display:flex; flex-direction: row;")(
      left.uiElement,
      divider,
      right.uiElement
    ).render

  var factor: Double = initFactor

  def setLeftRightWidths(w: Int): Unit = {
    val dividerWidth = jQuery(divider).width.toInt
    val leftWidth = (w * factor).toInt
    left.setWidth(leftWidth)
    right.setWidth(w - leftWidth - dividerWidth)
  }

  override def setWidth(w: Int): Unit = {
    super.setWidth(w)
    setLeftRightWidths(w)
  }

  override def setHeight(h: Int): Unit = {
    super.setHeight(h)
    left.setHeight(h)
    right.setHeight(h)
  }

  var pressed: Boolean = false
  var pressX: Int = 0

  def doMouseDown(e: JQueryEventObject): Unit =  {

    e.preventDefault()
    pressX = e.pageX
    pressed = true

  }

  def doMouseMove(e: JQueryEventObject): Unit = {
    e.preventDefault

    if (pressed) {
      val off = jQuery(uiElement).offset().asInstanceOf[Offset]
      val uiWidth = jQuery(uiElement).width()
      val offX = e.pageX - off.left
      factor = offX / uiWidth 
      setLeftRightWidths(uiWidth.toInt)
    }
  }
  
  def doMouseUp(ev: JQueryEventObject): Unit = {
    pressed = false
  }

  def initialize: Unit = {
    jQuery(divider).on("mousedown", (e: JQueryEventObject) => doMouseDown(e))
    jQuery(document).on("mousemove", doMouseMove(_))
    jQuery(document).on("mouseup", doMouseUp(_))
  }

}


