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

  val arrow: SComplex[Int] = 
    ||(SBox(2, STree.obj(SDot(1)))) >> SDot(3)

  val obj: SComplex[Int] = 
    ||(SDot(14))

  val twoGlob: SComplex[Int] = 
    arrow.glob(4, 5).get

  val twoGlobCard: SCardinal[Int] = 
    SCardinal(twoGlob)

  val threeGlob: SComplex[Int] = 
    twoGlob.glob(6, 7).get

  

  // mainViewer.gallery.onCellClick = 
  //   (c : mainViewer.gallery.ActiveBox) => { 

  //     // println("Testing face routine for: " + c.toString)
  //     // println("Address is: " + c.address.toString)
  //     // println("Dimension is: " + c.dim.toString)

  //     for {
  //       face <- fredSComplex.truncateToDim(c.dim).sourceAt(c.address)
  //     } {

  //       val viewer = new JsStableViewer[Int](face)
  //       jQuery("#face-viewer-div").empty().append(viewer.uiElement)
  //       viewer.gallery.renderAll

  //     }

  //   }

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main: Unit = {

    println("Launched Opetopic Javscript ...")

    // jQuery("#main-viewer-div").append(mainViewer.uiElement)
    // mainViewer.gallery.renderAll

    val editor = new JsStableEditor[Int]

    jQuery("#editor-div").append(editor.uiElement)
    // editor.editor.renderAll

  }

}
