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
import markers._
import JsDomFramework._

class SketchpadEditor extends JsCardinalEditor[SimpleMarker] {

  implicit val vf: VisualizableFamily[SimpleMarker] = 
    SimpleMarker.frameworkFamily(JsDomFramework)

}
