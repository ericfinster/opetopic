/**
  * SimpleMarker.scala - A Simple Cell Marker Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

case class SimpleMarker(
  val lbl: String,
  val colorSpec: ColorSpec = DefaultColorSpec
)

object SimpleMarker {

  implicit object SimpleMarkerRenderable extends Renderable[SimpleMarker] {
    def render(f: UIFramework)(mk: SimpleMarker): f.CellRendering = {

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

