/**
  * Router.scala - Site router
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs.js
import js.annotation._

import org.scalajs.jquery._
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.PopStateEvent

object Router {

  def initialize: Unit = {

    dom.window.onpopstate = {
      (e: PopStateEvent) => {
        val url = e.state.asInstanceOf[String]
        showPage(url)
      }
    }

  }

  def showPage(p: Page) : Unit = {

    jQuery(Opetopic.contentDiv).empty().append(p.content)
    dom.window.history.pushState(p.url, null, p.url)

  }

  def showPage(url: String) : Unit = 
    url match {
      case "/" => showPage(MainPage)
      case "/docs" => showPage(DocumentationPage)
    }

}



