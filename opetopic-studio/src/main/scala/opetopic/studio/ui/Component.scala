/**
  * Component.scala - An abstract UI Component
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.studio.ui

import org.scalajs.dom.html._
import org.scalajs.jquery._

abstract class Component {

  val uiElement : Element

  def setWidth(w: Int): Unit = {
    jQuery(uiElement).width(w)
  }

  def setHeight(h: Int): Unit = {
    jQuery(uiElement).height(h)
  }

}

case class PlainComponent(val uiElement: Element) extends Component 

