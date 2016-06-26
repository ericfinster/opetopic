/**
  * SimpleMarker.scala - A Simple Cell Marker Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic.SAddr

case class SimpleMarker(
  val lbl: String,
  val colorSpec: ColorSpec = DefaultColorSpec,
  val targetDec: Option[DecorationType] = None,
  val sourceDec: Map[SAddr, DecorationType] = Map()
)

sealed trait DecorationType
case class TriangleDec(val color: String) extends DecorationType


object SimpleMarker {

  implicit object SimpleMarkerRenderable extends Renderable[SimpleMarker] {
    def render(f: UIFramework)(mk: SimpleMarker): f.CellRendering = {

      import f._
      import isNumeric._

      implicit def intToUnit(i: Int) : Size =
        fromInt(i)

      def triUp(c: String) : PolygonType = polygon(c, 100, c, List((150, 0), (300, 300), (0, 300)))
      def triDown(c: String) : PolygonType = polygon(c, 100, c, List((0, 0), (150, 300), (300, 0)))
      def bnds : Bounds = Bounds(0, 0, 300, 300)

      val lblEl = 
        if (mk.lbl == "")
          spacer(Bounds(fromInt(0), fromInt(0), fromInt(600), fromInt(600)))
        else text(mk.lbl)

      val td = mk.targetDec.map({
        case TriangleDec(c) => BoundedElement(triDown(c), bnds)
      })

      CellRendering(lblEl, mk.colorSpec, td, Map())

    }
  }

}

