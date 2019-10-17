/**
  * VerticalSplitPane.scala - A vertical split pane
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.ui

import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js
import org.scalajs.dom.window.document

class VerticalSplitPane(val top: Component, val bottom: Component, initFactor: Double = 0.5) extends Component {

  val divider = div(id := "divider",
    style := "min-height: 5px; background: #36383a; cursor: ns-resize; user-drag: none;"
  ).render

  val uiElement =
    div(
      top.uiElement,
      divider,
      bottom.uiElement
    ).render

  var factor: Double = initFactor

  def setTopBottomHeights(h: Int): Unit = {
    val dividerHeight = jQuery(divider).height.toInt
    val topHeight = (h * factor).toInt
    top.setHeight(topHeight)
    bottom.setHeight(h - topHeight - dividerHeight)
  }

  override def setWidth(w: Int): Unit = {
    super.setWidth(w)
    top.setWidth(w)
    bottom.setWidth(w)
  }

  override def setHeight(h: Int): Unit = {
    super.setHeight(h)
    setTopBottomHeights(h)
  }

  var pressed: Boolean = false
  var pressY: Int = 0

  def doMouseDown(e: JQueryEventObject): Unit =  {

    e.preventDefault()
    pressY = e.pageY
    pressed = true

  }

  def doMouseMove(e: JQueryEventObject): Unit = {
    e.preventDefault

    if (pressed) {
      val off = jQuery(uiElement).offset().asInstanceOf[Offset]
      val uiHeight = jQuery(uiElement).height()
      val offY = e.pageY - off.top
      factor = offY / uiHeight 
      setTopBottomHeights(uiHeight.toInt)
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


