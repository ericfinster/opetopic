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
import JsDomFramework._

import opetopic._
import opetopic.ui._

object Opetopic extends JSApp {

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main: Unit = {

    println("Launched Opetopic Javscript ...")
    println("Going to do some stable experiments ...")

    val editor = new StableEditor
    jQuery("#editor-div").append(editor.uiElement)
    editor.initialize

  }

  class StableEditor extends JsCardinalEditor[ConstString] {

    implicit val vf = VisualizableFamily.constStringVisualizable

  }

}


