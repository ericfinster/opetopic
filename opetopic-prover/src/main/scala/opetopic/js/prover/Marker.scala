/**
  * Marker.scala - Markers for attaching to editor cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import opetopic._
// import opetopic.js._
import opetopic.ott.OttSyntax._
import opetopic.ui._

case class Marker(
  val wksp: DefinitionWorkspace,
  val displayName: String,
  val expr: ExpT
) {

  def idExpr: ExpT =
    EVar(displayName)
  
  def cs : ColorSpec =
    if (wksp.hasUniversalProperty(displayName)) {
      FillColorSpec
    } else 
      expr match {
        case EVar(_) => VarColorSpec
        case _ => CompColorSpec
      }
  
}

object VarColorSpec extends ColorSpec(
  fill = "#FFF8DB",
  fillHovered = "#FFE21F",
  fillSelected = "#FBBD08",
  stroke = "#000000",
  strokeHovered = "#000000",
  strokeSelected = "#000000"
)

object CompColorSpec extends ColorSpec(
  fill = "#FFE8E6",
  fillHovered = "#FF695E",
  fillSelected = "#DB2828",
  stroke = "#000000",
  strokeHovered = "#000000",
  strokeSelected = "#000000"
)

object FillColorSpec extends ColorSpec(
  fill = "#DFF0FF",
  fillHovered = "#54C8FF",
  fillSelected = "#2185D0",
  stroke = "#000000",
  strokeHovered = "#000000",
  strokeSelected = "#000000"
)

object Marker {


  implicit object MarkerRenderable extends Renderable[Marker] {
    def render(f: UIFramework)(mk: Marker): f.CellRendering = {

      import f._
      import isNumeric._

      implicit def intToUnit(i: Int) : Size =
        fromInt(i)

      val lblEl = 
        if (mk.displayName == "")
          spacer(Bounds(0, 0, 600, 600))
        else text(mk.displayName)

      // val td = mk.targetDec.map(_.render(f))
      // val sd = mk.sourceDec.mapValues(_.render(f))

      CellRendering(lblEl, mk.cs, None, Map())

    }
  }

}
