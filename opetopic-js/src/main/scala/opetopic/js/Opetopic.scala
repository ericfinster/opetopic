/**
  * Opetopic.scala - Main site entry and setup
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs.js.JSApp

import org.scalajs.jquery._
import org.scalajs.dom
import scalatags.JsDom.all._
import JQuerySemanticUI._

object Opetopic extends JSApp {

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main: Unit = {

    println("Launched Opetopic ...")

    // jQuery(".ui.dropdown").dropdown()

    // val appname = jQuery("meta[name=application]").attr("content").toString
    // println("App is: " ++ appname)
    // appname match {
    //   case "prover" => {
    //     addScript("opetopicprover-fastopt.js")
    //     addScript("opetopicprover-launcher.js")
    //   }
    //   case "sketchpad" => {
    //     addScript("opetopicsketchpad-fastopt.js")
    //     addScript("opetopicsketchpad-launcher.js")
    //   }
    // }


  }

  // It would be nicer, I think, to do this via ajax and
  // not have to break the content security policy
  // (see the "unsafe-eval" property in application.conf
  // def addScript(name: String) = {

  //   val scr = dom.document.createElement("script")
  //   scr.setAttribute("type", "text/javascript")
  //   scr.setAttribute("src", "assets/" + name)
  //   dom.document.getElementsByTagName("body")(0).appendChild(scr)

  // }

}


