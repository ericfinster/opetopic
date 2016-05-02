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

object Opetopic extends JSApp {

  val editor = new StableEditor
  val viewer = new StableViewer

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main: Unit = {

    println("Launched Opetopic Javscript ...")
    println("Going to do some stable experiments ...")

    jQuery("#editor-div").append(editor.uiElement)
    editor.initialize

    jQuery("#viewer-div").empty().append(viewer.uiElement)
    viewer.initialize

    jQuery("#action-btn").on("click", () => { doAction })

  }

  class StableEditor extends JsCardinalEditor[ConstString] {
    implicit val vf = VisualizableFamily.constStringVisualizable
  }

  type OptString[N <: Nat] = Option[String]

  class StableViewer extends JsComplexViewer[OptString] {
    implicit val vf = 
      VisualizableFamily.optionVisualizableFamily(
        Bounds(0, 0, 600, 600),
        VisualizableFamily.constStringVisualizable
      )
  }

  //============================================================================================
  // STABLE EXPERIMENTS
  //

  def doAction: Unit = {

    def stringRender: Option[String] => BoundedElement = 
      _ => spacer(Bounds(0,0,600,600))

    for {
      fc <- fromOpt(editor.selectedFaceComplex, ShapeError("Nothing selected"))
      // builder = new ComplexBuilder[String]
      // stableNst <- builder.fromComplex(fc.n)(fc.value)
      // _ = println("Successfully stabilized")
      // readbackCmplx <- builder.toComplex(fc.n)(stableNst.baseValue)
    } {

      val complex = new ActiveComplex[String](ComplexConfig(), stringRender)

      for {
        _ <- complex.ActiveComplexBuilder.fromComplex(fc.n)(fc.value)
      } {
        println("Successfully loaded a complex")
      }

      // viewer.complex = Some(readbackCmplx)
      // println("Action successful")

    }

  }


}


