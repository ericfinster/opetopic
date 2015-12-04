/**
  * SvgExporter.scala - Simple Class for Exporting SVG's
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

import opetopic._
import opetopic.ui._
import opetopic.tt._
import syntax.complex._

import Cell._
import ScalatagsTextFramework._

class SvgExporter[N <: Nat](cmplx: Complex[OptCell, N]) {

  implicit val staticPanelConfig =
    PanelConfig(
      internalPadding = 400,
      externalPadding = 600,
      leafWidth = 200,
      strokeWidth = 100,
      cornerRadius = 200
    )

  implicit val staticGalleryConfig =
    GalleryConfig(
      panelConfig = staticPanelConfig,
      width = 1000,
      height = 300,
      spacing = 2000,
      minViewX = Some(80000),
      minViewY = Some(15000),
      spacerBounds = Bounds(0, 0, 600, 600)
    )

  implicit object StaticCellAffixableFamily extends AffixableFamily[Cell] {
    def apply[N <: Nat](n: N) : Affixable[Cell[N]] =
      new Affixable[Cell[N]] {
        type ElementType = TextType
        def decoration(cell: Cell[N]) =
          cell.isLeftExt match {
            case None =>
              cell.expr match {
                case EVar(_) => Decoration(text(cell.id), "variable")
                case _ => Decoration(text(cell.id), "composite")
              }
            case Some(_) => Decoration(text(cell.id), "universal")
          }
      }
  }

  implicit val spacerBounds = Bounds(0, 0, 10, 10)

  val staticGallery = SimpleStaticGallery(cmplx)

  def svgString = staticGallery.element.toString

}
