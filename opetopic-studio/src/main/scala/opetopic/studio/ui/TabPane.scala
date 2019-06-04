/**
  * TabPane.scala - A pane for switching between multiple tabs...
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.studio.ui

import scala.collection.mutable.Buffer

import org.scalajs.dom.Element
import org.scalajs.jquery._
import scalatags.JsDom.all._

class Tab(val name: String, val child: Component, active: Boolean = false) extends Component {

  val uiElement =
    div(cls := "ui" ++ (if (active) " active" else "") ++ " tab" ,
      attr("data-tab") := name)(child.uiElement).render

  override def setWidth(w: Int): Unit = {
    super.setWidth(w)
    child.setWidth(w)
  }

  override def setHeight(h: Int): Unit = {
    super.setHeight(h)
    child.setHeight(h)
  }

}


class TabPane(t : Tab*) extends Component {

  private val tabs: Buffer[Tab] = Buffer(t : _*)

  val uiElement =
    div(t.map(_.uiElement : scalatags.JsDom.Modifier) : _*).render

  def addTab(t: Tab): Unit = {
    tabs += t
    jQuery(uiElement).append(t.uiElement)
    
  }

  def removeTab(t: Tab): Unit = {
    tabs -= t
    jQuery(t.uiElement).remove()
  }

  override def setWidth(w: Int): Unit = {
    super.setWidth(w)
    tabs.foreach(_.setWidth(w))
  }

  override def setHeight(h: Int): Unit = {
    super.setHeight(h)
    tabs.foreach(_.setHeight(h))
  }

}
