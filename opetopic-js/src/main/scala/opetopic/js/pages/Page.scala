/**
  * Page.scala - Page trait
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.pages

import org.scalajs.dom
import org.scalajs.dom.Element

trait Page {

  def url: String
  def render: Element

  // Are we going to use these ...
  def onLoad: Unit = ()
  def onShow: Unit = ()

  var loaded: Boolean = false

}

