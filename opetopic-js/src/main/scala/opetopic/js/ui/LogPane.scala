/**
  * LogPane.scala - A simple logger
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.ui

import org.scalajs.dom.html._
import org.scalajs.jquery._
import scalatags.JsDom.all._

class LogPane extends Component {

  val uiElement =
    div(cls := "ui inverted vertical menu", style := "overflow-y: auto; overflow-x: auto;").render

  def log(str: String): Unit = {
    jQuery(uiElement).append(div(cls := "item")(str).render)
  }

  def ok(str: String): Unit = {
    jQuery(uiElement).append(div(cls := "item", color := "green")(str).render)
  }

  def error(str: String): Unit = {
    jQuery(uiElement).append(div(cls := "item", color := "red")(str).render)
  }

  def debug(str: String): Unit = {
    jQuery(uiElement).append(div(cls := "item", color := "yellow")(str).render)
  }

  
  // Add a clear button?
  // Add colors for different states ...

}
