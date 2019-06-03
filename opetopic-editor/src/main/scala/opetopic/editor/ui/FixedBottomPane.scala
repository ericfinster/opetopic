/**
  * BottomMenuPane.scala - A pane which uses the bottom component for sizing
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.editor.ui

import org.scalajs.jquery._
import scalatags.JsDom.all._

class FixedBottomPane(val top: Component, val bottom: Component) extends Component {

  val uiElement =
    div(top.uiElement, bottom.uiElement).render

  override def setWidth(w: Int): Unit = {
    super.setWidth(w)
    top.setWidth(w)
    bottom.setWidth(w)
  }

  override def setHeight(h: Int): Unit = {
    super.setHeight(h)
    top.setHeight(h - jQuery(bottom.uiElement).height.toInt)
  }

}
