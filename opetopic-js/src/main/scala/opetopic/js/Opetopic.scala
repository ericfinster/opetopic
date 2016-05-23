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

  val arrow: SComplex[Int] = 
    ||(SBox(2, STree.obj(SDot(1)))) >> SDot(3)

  // val twoGlob: SComplex[Int] = 
  //   ||(SBox(2, STree.obj(SDot(1)))) >>
  //     SBox(4, SNode(SDot(3), STree.obj(SLeaf))) >>
  //     SDot(5)

  val twoGlob: SComplex[Int] = 
    arrow.glob(4, 5).get

  val twoGlobCard: SCardinal[Int] = 
    SCardinal(twoGlob)

  val editor = new JsStableEditor[Int](twoGlob)

  // import scalaz.Traverse
  // import scalaz.syntax.traverse._

  // println("As a complex: " + editor.editor.complex.map(_.label).toString)

  // val cardComplex: SComplex[Polarity[Option[Int]]] = 
  //   editor.editor.complex.map(_.label)

  // val mainViewer = new JsStableViewer[Polarity[Option[Int]]](cardComplex)

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
