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

  def main : Unit = {

    println("Launched Opetopic CodeBuilder.")

    jQuery(".ui.dropdown.button").dropdown(
      js.Dynamic.literal(on = "hover")
    )

    jQuery("#new-definition").click((e: JQueryEventObject) => {
      println("New Definition!")
    })

    jQuery("#new-codeblock").click((e: JQueryEventObject) => {
      println("New code block ...")
      addCodeBlock
    })

    addCodeBlock

  }


}
