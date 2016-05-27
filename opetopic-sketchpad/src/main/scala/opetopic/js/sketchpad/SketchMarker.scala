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

      import f._
      import isNumeric._

      val lblEl = 
        if (mk.lbl == "")
          spacer(Bounds(fromInt(0), fromInt(0), fromInt(600), fromInt(600)))
        else text(mk.lbl)


      CellRendering(lblEl, mk.colorSpec)

    }
  }

}

