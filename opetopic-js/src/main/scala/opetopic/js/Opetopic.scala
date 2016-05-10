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

object Test {

  // Okay, this setup seems to work, and it looks like we'll be able
  // to go ahead and add the visual elements to the dom.  So let's 
  // move ahead with reorganizing the class hierarchy and try to get
  // back to where we were before.

  // object TestConfig extends StablePanelConfig[JsDomFramework.type] {

  //   val framework = JsDomFramework

  //   def internalPadding : Int = 0
  //   def externalPadding : Int = 0
  //   def leafWidth : Int = 0
  //   def strokeWidth : Int = 0
  //   def cornerRadius : Int = 0

  // }

  // class TestPanel extends StablePanel[String, JsDomFramework.type] {

  //   type CellType = TestCell

  //   val config = TestConfig

  //   def bounds: Bounds = Bounds()
  //   def element: Element = spacer(Bounds()).element

  //   class TestCell extends PanelCell {

  //     def label: String = "Hello, world!"
  //     def labelBounds: Bounds = Bounds()

  //   }

  // }

}
