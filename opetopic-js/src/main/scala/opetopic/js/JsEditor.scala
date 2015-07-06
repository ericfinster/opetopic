/**
  * JsEditor.scala - A Cardinal Editor in JavaScript
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs._
import org.scalajs.dom

object JsEditor extends js.JSApp {

  def main : Unit = {

    val mainContent = dom.document.getElementById("main")

    println("Started ...")

    dom.document.addEventListener("onclick", documentEventLoop, true)

    println("Installed event handler ...")

  }

  def documentEventLoop : dom.Event => Unit = { ev =>
    println("Got an event!")
  }

}
