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
import opetopic.stable._
import syntax.nesting._
import syntax.complex._
import opetopic.Examples._

object Opetopic extends JSApp {

  val faceViewer = new JsStableViewer[Int]
  val mainViewer = new JsStableViewer[Int]

  val editor = new JsStableEditor[String]

  mainViewer.gallery.onCellClick = 
    (c : mainViewer.gallery.ActiveCell) => { faceViewer.loadFace(c) }

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main: Unit = {

    println("Launched Opetopic Javscript ...")

    jQuery("#main-viewer-div").append(mainViewer.uiElement)
    jQuery("#face-viewer-div").append(faceViewer.uiElement)

    jQuery("#editor-div").append(editor.uiElement)

    mainViewer.loadComplex(fredComplex)

    editor.editor.renderAll

  }

}
