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

  type OptString[N <: Nat] = Option[String]

  val fredStrComplex: Complex[OptString, _4] =
    fredComplex.map(new IndexedMap[ConstInt, OptString] {
      def apply[N <: Nat](n: N)(i: Int) : Option[String] = Some(i.toString)
    })

  val faceViewer = new JsStableViewer

  val mainViewer = new JsStableViewer
  mainViewer.gallery.onCellClick = 
    (c : mainViewer.gallery.ActiveCell) => {

      println("Attempting face for cell " + c.label.toString)

      faceViewer.gallery.panels.clear

      for {
        topCell <- c.face(faceViewer.gallery.StableFactory)
      } {

        topCell.targets.reverse.foreach(t => {
          faceViewer.gallery.panels += new faceViewer.gallery.ActivePanel(t)
        })


        faceViewer.gallery.initialize
        faceViewer.gallery.refreshAll

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

    jQuery(faceViewer.uiElement).append(faceViewer.gallery.uiElement)

    mainViewer.loadComplex(fredStrComplex)

  }


}


