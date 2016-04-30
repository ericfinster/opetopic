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
import opetopic.mutable._
import syntax.nesting._
import syntax.complex._

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

    jQuery("#action-btn").on("click", () => { doAction(editor) })

  }

  class StableEditor extends JsCardinalEditor[ConstString] {

    implicit val vf = VisualizableFamily.constStringVisualizable

    override def onSelect[N <: Nat](n: N)(editor: CardinalEditor[ConstString])(box: editor.CardinalCellBox[N]): Unit = {
    }

  }

  //============================================================================================
  // STABLE EXPERIMENTS
  //

  def doAction(editor: StableEditor): Unit = {

    for {
      fc <- fromOpt(editor.selectedFaceComplex, ShapeError("Nothing selected"))
      builder = new StableCell.ComplexBuilder[String]
      result <- builder.fromComplex(fc.n)(fc.value)
    } {

      val topCell = result.baseValue

      println("Stable cell was built successfully.")

    }

  }


}


