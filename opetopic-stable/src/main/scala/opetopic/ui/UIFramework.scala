/**
  * UIFramework.scala - Generic UI Backend for Opetopic Displays
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic.SAddr

abstract class UIFramework {

  type Size
  type Element

  type PathType <: Element
  type TextType <: Element
  type GroupType <: Element
  type PolygonType <: Element
  type ViewportType <: Element
  type RectangleType <: Element

  implicit val isNumeric: Numeric[Size]
  implicit val isOrdered: Ordering[Size]

  import isNumeric._

  //============================================================================================
  // CANVAS ELEMENTS
  //

  def viewport(width: Size, height: Size, bounds: Bounds, elem: Element*) : ViewportType
  def group(elem: Element*) : GroupType
  def polygon(stroke: String, strokeWidth: Size, fill: String, pts: List[(Size, Size)]) : PolygonType
  def rect(x: Size, y: Size, width: Size, height: Size, r: Size, stroke: String, strokeWidth: Size, fill: String) : RectangleType
  def path(d: String, stroke: String, strokeWidth: Size, fill: String) : PathType
  def text(str: String) : BoundedElement
  def toast(str: String) : Unit = ()

  def spacer(bnds: Bounds) : BoundedElement = 
    BoundedElement(group(), bnds)

  case class Transform(
    val translateX : Size,
    val translateY : Size,
    val scaleX : Size,
    val scaleY : Size
  ) 

  def transform(el: Element, t: Transform) : Element
  def translate(el: Element, x: Size, y: Size) : Element
  def scale(el: Element, x: Size, y: Size) : Element

  def makeMouseInvisible(el: Element) : Element

  //============================================================================================
  // BOUNDS AND SIZES
  //

  def half(size: Size) : Size

  case class Bounds(
    val x: Size = zero,
    val y: Size = zero,
    val width: Size = zero,
    val height: Size = zero
  ) {

    def dimString: String = 
      x.toString ++ " " ++ y.toString ++ " " ++
        width.toString ++ " " ++ height.toString

  }

  trait BoundedElement {
    def element: Element
    def bounds: Bounds
  }

  object BoundedElement {

    def apply(el: Element, bnds: Bounds) = 
      new BoundedElement {
        val element = el
        val bounds = bnds
      }

  }

  case class CellRendering(
    val boundedElement: BoundedElement,
    val colorSpec: ColorSpec = DefaultColorSpec,
    val targetDec: Option[BoundedElement] = None,
    val sourceDec: Map[SAddr, BoundedElement] = Map()
  )

  //============================================================================================
  // TEXT RENDERING
  //

  def renderTextGroup(str: String, font: ScalaSVGFont, stroke: String, strokeWidth: Size, fill: String) : (GroupType, Bounds) = {

    var advance : Size = zero
    var ascent : Size = zero
    var descent : Size = zero

    val glyphMap = font.glyphMap

    val paths : Seq[Element] = (str map (c => {

      if (glyphMap.isDefinedAt(c)) {

        val glyph = glyphMap(c)

        val p = transform(path(glyph.pathStr, stroke, strokeWidth, fill),
          Transform(advance, zero, fromInt(1), fromInt(-1)))

        advance += fromInt(glyph.advance)
        ascent = isOrdered.max(ascent, fromInt(glyph.ascent))
        descent = isOrdered.max(descent, fromInt(glyph.descent))

        Some(p)

      } else None

    })).flatten

    (group(paths: _*), Bounds(zero, -ascent, advance, ascent + descent))

  }

}
