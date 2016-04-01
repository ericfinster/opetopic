/**
  * Page.scala - Page trait
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import org.scalajs.dom
import org.scalajs.dom.Element

import scalatags.JsDom.all._

trait Page {

  def url: String
  def content: Element

  def onShow: Unit = ()

}

object MainPage extends Page {

  val url = "/"

  val content = 
    p("This is the main page").render

}

object DocumentationPage extends Page {

  val url = "/docs"

  val content = 
    p("This is the documentation page").render

}
