/**
  * CodeBuilder.scala - Opetopic CodeBuilder Application
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.jquery._

import opetopic.js.JQuerySemanticUI._

object CodeBuilder extends JSApp {

  def addCodeBlock: Unit = {
    val pane = new CodeBlockPane
    jQuery("#panes").append(pane.uiElement)
    pane.initialize
  }

  def addDesignBlock: Unit = {
    val pane = new DesignBlockPane
    jQuery("#panes").append(pane.uiElement)

    jQuery(pane.sidebarElement).sidebar(
      js.Dynamic.literal(
        context = ".bottom.segment",
        dimPage = false,
        transition = "overlay",
        closable = false
      )
    )

  }

  def main : Unit = {

    println("Launched Opetopic CodeBuilder.")

    jQuery(".ui.dropdown.button").dropdown(
      js.Dynamic.literal(on = "hover")
    )

    jQuery("#new-designblock").click((e: JQueryEventObject) => {
      addDesignBlock
    })

    jQuery("#new-codeblock").click((e: JQueryEventObject) => {
      addCodeBlock
    })

    // addCodeBlock

  }


}
