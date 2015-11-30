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
import scala.scalajs.js.Dynamic.{literal => lit}

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
    jQuery(pane.accordion).accordion()
    jQuery(pane.uiElement).find(".ui.dropdown").dropdown(lit(on = "hover"))
  }

  def main : Unit = {

    println("Launched Opetopic CodeBuilder.")

    jQuery("#new-designblock").click((e: JQueryEventObject) => {
      addDesignBlock
    })

    jQuery("#new-codeblock").click((e: JQueryEventObject) => {
      addCodeBlock
    })

    // addCodeBlock

  }


}
