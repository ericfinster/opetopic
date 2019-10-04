/**
  * HorizontalSplitPane.scala - A horizontal split pane
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.ui

import org.scalajs.jquery._
import scalatags.JsDom.all._

class HorizontalSplitPane(val left: Component, val right: Component) extends Component {

  val divider = div(id := "divider",
    style := "min-width: 5px; background: #36383a; cursor: ns-resize;"
  ).render

  val uiElement =
    div(style := "display:flex; flex-direction: row;")(
      left.uiElement,
      divider,
      right.uiElement
    ).render

  override def setWidth(w: Int): Unit = {
    super.setWidth(w)

    val dividerWidth = jQuery(divider).width.toInt
    val halfW = (w / 2).toInt

    left.setWidth(halfW)
    right.setWidth(w - halfW - dividerWidth)

  }

  override def setHeight(h: Int): Unit = {
    super.setHeight(h)
    left.setHeight(h)
    right.setHeight(h)
  }

}


