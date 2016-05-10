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

  val fredStrComplex: Complex[ConstString, _4] =
    fredComplex.map(new IndexedMap[ConstInt, ConstString] {
      def apply[N <: Nat](n: N)(i: Int) : String = i.toString
    })

  val faceViewer = new JsStableViewer[String]
  val mainViewer = new JsStableViewer[String]

  mainViewer.gallery.onCellClick = 
    (c : mainViewer.gallery.ActiveCell) => {

      println("Attempting face for cell " + c.label.toString)

      faceViewer.gallery.myPanels.clear

      for {
        topCell <- c.face(faceViewer.gallery.SimpleFactory)
      } {

        topCell.targets.reverse.foreach(t => {
          faceViewer.gallery.myPanels += faceViewer.gallery.SimpleActivePanel(t)
        })

        faceViewer.gallery.renderAll

        println("Face gallery updated")

      }

    }

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main: Unit = {

    println("Launched Opetopic Javscript ...")

    jQuery("#main-viewer-div").append(mainViewer.uiElement)
    jQuery("#face-viewer-div").append(faceViewer.uiElement)

    jQuery(faceViewer.uiElement).append(faceViewer.gallery.element.uiElement)

    mainViewer.loadComplex(fredStrComplex)

  }


}
