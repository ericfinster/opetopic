/**
  * Opetopic.scala - Main site entry and setup
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs.js.JSApp

import org.scalajs.jquery._
import scalatags.JsDom.all._
import JQuerySemanticUI._

object Opetopic extends JSApp {

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main: Unit = {

    println("Launched opetopic ...")

    jQuery(".ui.dropdown").dropdown()

    val appname = jQuery("meta[name=application]").attr("content").toString

    appname match {
      case "prover" => jQuery().getScript("/assets/opetopicprover-launcher.js")
      case "sketchpad" => jQuery().getScript("/assets/opetopicsketchpad-launcher.js")
    }

  }

}


