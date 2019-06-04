/**
  * VerticalSplitPane.scala - A vertical split pane
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.studio.ui

import org.scalajs.jquery._
import scalatags.JsDom.all._

class VerticalSplitPane(val top: Component, val bottom: Component) extends Component {

  val divider = div(id := "divider",
    style := "min-height: 5px; background: #36383a; cursor: ns-resize;"
  ).render

  val uiElement =
    div(
      top.uiElement,
      divider,
      bottom.uiElement
    ).render

  override def setWidth(w: Int): Unit = {
    super.setWidth(w)
    top.setWidth(w)
    bottom.setWidth(w)
  }

  override def setHeight(h: Int): Unit = {
    super.setHeight(h)

    val dividerHeight = jQuery(divider).height.toInt
    val halfH = (h / 2).toInt

    top.setHeight(halfH)
    bottom.setHeight(h - halfH - dividerHeight)
  }

}


