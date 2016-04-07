/**
  * SketchpadViewer.scala - Sketchpad Viewer Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import opetopic._
import opetopic.js._
import opetopic.ui._
import markers._
import JsDomFramework._
import SimpleMarker._

class SketchpadViewer extends JsComplexViewer[OptMarker] {

  implicit val vf: VisualizableFamily[OptMarker] = 
    VisualizableFamily.optionVisualizableFamily(
      Bounds(0, 0, 600, 600),
      SimpleMarker.frameworkFamily(JsDomFramework)
    )

}
