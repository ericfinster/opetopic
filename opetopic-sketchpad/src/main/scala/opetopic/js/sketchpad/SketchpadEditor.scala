/**
  * SketchpadEditor.scala - Custom Editor for the Sketchpad
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import org.scalajs.dom

import opetopic._
import opetopic.js._
import opetopic.ui._
import syntax.complex._
import markers._
import JsDomFramework._

class SketchpadEditor extends JsCardinalEditor[SimpleMarker] {

  implicit val vf: VisualizableFamily[SimpleMarker] = 
    SimpleMarker.frameworkFamily(JsDomFramework)

  //============================================================================================
  // SELECTION HANDLERS
  //

  override def onObjectSelect(editor: CardinalEditor[SimpleMarker])(box: editor.CardinalCellBox[_0]) : Unit =
    for {
      lc <- box.labelComplex
    } {

      println("Displaying object ...")
      Sketchpad.viewer.complex = Some(lc)

    }

  override def onCellSelect[P <: Nat](editor: CardinalEditor[SimpleMarker])(box: editor.CardinalCellBox[S[P]]) : Unit =
    for {
      lc <- box.labelComplex
    } {

      println("Displaying cell ...")
      Sketchpad.viewer.complex = Some(lc)

    }

}
