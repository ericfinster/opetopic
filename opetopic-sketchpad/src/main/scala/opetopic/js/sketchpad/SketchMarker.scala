/**
  * SketchMarker.scala - A Simple Marker for the Sketchpad
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import opetopic.ui._

case class SketchMarker(
  val lbl: String,
  val colorSpec: ColorSpec = DefaultColorSpec
)

object SketchMarker {

  implicit object SketchMarkerRenderable extends Renderable[SketchMarker] {
    def render(f: UIFramework)(mk: SketchMarker): f.CellRendering = {
      f.CellRendering(f.text(mk.lbl), mk.colorSpec)
    }
  }

}

