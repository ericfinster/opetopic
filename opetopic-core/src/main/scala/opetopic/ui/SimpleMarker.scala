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
  val targetDec: Option[EdgeDecoration] = None,
  val sourceDec: Map[SAddr, EdgeDecoration] = Map()
) {

  def withLabel(l: String): SimpleMarker = 
    this.copy(lbl = l)

  def withStroke(s: String): SimpleMarker = 
    this.copy(colorSpec = this.colorSpec.withStroke(s))

  def withFill(f: String): SimpleMarker = 
    this.copy(colorSpec = this.colorSpec.withFill(f))

  def withTargetDec(d: Option[EdgeDecoration]) = 
    this.copy(targetDec = d)

  def addSourceDec(addr: SAddr, d: EdgeDecoration) = 
    this.copy(sourceDec = this.sourceDec + (addr -> d))

  def removeSourceDec(addr: SAddr) = 
    this.copy(sourceDec = this.sourceDec - addr)

  override def toString = lbl
  
}

case class EdgeDecoration(val shape: String, val color: String, val tgt: Boolean = true) {

  def render(f: UIFramework): f.BoundedElement = {

    import f._
    import isNumeric._

    implicit def intToUnit(i: Int) : Size =
      fromInt(i)

    def triUp(c: String) : PolygonType = polygon(c, 100, c, List((150, 0), (300, 300), (0, 300)))
    def triDown(c: String) : PolygonType = polygon(c, 100, c, List((0, 0), (150, 300), (300, 0)))

    def circle(c: String): RectangleType = 
      rect(0, 0, 300, 300, 200, c, 100, c)

    def square(c: String): RectangleType = 
      rect(0, 0, 300, 300, 0, c, 100, c)

    def bnds : Bounds = Bounds(0, 0, 300, 300)

    shape match {
      case "triangle" => 
        if (tgt) BoundedElement(triDown(color), bnds) else
          BoundedElement(triUp(color), bnds)
      case "circle" => BoundedElement(circle(color), bnds)
      case _ => BoundedElement(square(color), bnds)
    }

  }

}

object SimpleMarker {

  implicit object SimpleMarkerRenderable extends Renderable[SimpleMarker] {
    def render(f: UIFramework)(mk: SimpleMarker): f.CellRendering = {

      import f._
      import isNumeric._

      implicit def intToUnit(i: Int) : Size =
        fromInt(i)

      val lblEl = 
        if (mk.lbl == "")
          spacer(Bounds(0, 0, 600, 600))
        else text(mk.lbl)

      val td = mk.targetDec.map(_.render(f))
      val sd = mk.sourceDec.mapValues(_.render(f))

      CellRendering(lblEl, mk.colorSpec, td, sd)

    }
  }

  implicit object SimpleMarkerHasDefault extends HasDefault[SimpleMarker] {
    val default: SimpleMarker =
      SimpleMarker("")
  }

}

