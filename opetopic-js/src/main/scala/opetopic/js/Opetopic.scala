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

  val fredSComplex: SComplex[Int] = 
    SComplex(fredComplex)

  val testComplex: SComplex[Int] = 
    ||(SBox(4, STree.obj(SDot(3)))) >> SDot(7)

  val editor = new JsStableEditor[Int](testComplex)

  // val mainViewer = new JsStableViewer[Int](fredSComplex)

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

    jQuery("#editor-div").append(editor.uiElement)
    editor.editor.renderAll

  }

}
