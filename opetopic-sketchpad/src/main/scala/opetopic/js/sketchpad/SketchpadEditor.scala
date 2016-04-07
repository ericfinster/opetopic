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

  override def onObjectSelect(editor: CardinalEditor[SimpleMarker])(box: editor.CardinalCellBox[_0]) : Unit = showFace
  override def onCellSelect[P <: Nat](editor: CardinalEditor[SimpleMarker])(box: editor.CardinalCellBox[S[P]]) : Unit = showFace

  def showFace: Unit = 
    for {
      instance <- activeEditor
      boxsig <- instance.rootBox
      lc <- boxsig.value.labelComplex
    } { Sketchpad.viewer.complex = Some(lc) }

}
