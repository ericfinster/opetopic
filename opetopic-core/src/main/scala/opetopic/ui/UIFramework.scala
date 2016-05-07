/**
  * UIFramework.scala - Generic UI Backend for Opetopic Displays
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._

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

  //============================================================================================
  // VISUALIZABLE TYPECLASS
  //

  sealed trait Visualization[N <: Nat] {
    def colorSpec: ColorSpec
    def labelElement: BoundedElement
  }

  object Visualization {

    @natElim
    def apply[N <: Nat](n: N)(c: ColorSpec, le: BoundedElement) : Visualization[N] = {
      case (Z, c, le) => ObjectVisualization(c, le)
      case (S(p), c, le) => CellVisualization(c, le)
    }

  }

  case class ObjectVisualization(
    val colorSpec: ColorSpec,
    val labelElement: BoundedElement
  ) extends Visualization[_0]

  case class CellVisualization[P <: Nat](
    val colorSpec: ColorSpec,
    val labelElement: BoundedElement,
    val rootEdgeElement: Option[BoundedElement] = None,
    val leafEdgeElements: Option[Tree[Option[BoundedElement], P]] = None
  ) extends Visualization[S[P]]

  trait Visualizable[A, N <: Nat] {
    def visualize(a: A) : Visualization[N]
  }

  trait VisualizableFamily[A[_ <: Nat]] {
    def visualize[N <: Nat](n: N)(a: A[N]) : Visualization[N]
  }

  object VisualizableFamily {

    implicit def optionVisualizableFamily[A[_ <: Nat]](implicit bnds : Bounds, av: VisualizableFamily[A])
        : VisualizableFamily[Lambda[`N <: Nat` => Option[A[N]]]] =
      new VisualizableFamily[Lambda[`N <: Nat` => Option[A[N]]]] {
        def visualize[N <: Nat](n: N)(o: Option[A[N]]) : Visualization[N] = 
          o match {
            case None => Visualization(n)(DefaultColorSpec, spacer(bnds))
            case Some(a) => av.visualize(n)(a)
          }
      }

    implicit def polarityVisualizableFamily[A[_ <: Nat]](implicit av: VisualizableFamily[A])
        : VisualizableFamily[Lambda[`N <: Nat` => Polarity[A[N]]]] =
      new VisualizableFamily[Lambda[`N <: Nat` => Polarity[A[N]]]] {
        def visualize[N <: Nat](n: N)(p: Polarity[A[N]]) : Visualization[N] = 
          p match {
            case Positive() => Visualization(n)(PolarityColorSpec, text("+"))
            case Negative() => Visualization(n)(PolarityColorSpec, text("-"))
            case Neutral(a) => av.visualize(n)(a)
          }
      }

    implicit def poloptVisualizableFamily[A[_ <: Nat]](implicit bnds: Bounds, av: VisualizableFamily[A])
        : VisualizableFamily[Lambda[`N <: Nat` => Polarity[Option[A[N]]]]] =
      new VisualizableFamily[Lambda[`N <: Nat` => Polarity[Option[A[N]]]]] {
        def visualize[N <: Nat](n: N)(p: Polarity[Option[A[N]]]) : Visualization[N] = 
          polarityVisualizableFamily[Lambda[`K <: Nat` => Option[A[K]]]](
            optionVisualizableFamily[A](bnds, av)
          ).visualize(n)(p)
      }

    implicit val constStringVisualizable : VisualizableFamily[ConstString] = 
      new VisualizableFamily[ConstString] {
        def visualize[N <: Nat](n: N)(str: String) : Visualization[N] = 
          Visualization(n)(DefaultColorSpec, text(str))
      }

    implicit val visVisualizableFamily : VisualizableFamily[Visualization] = 
      new VisualizableFamily[Visualization] {
        def visualize[N <: Nat](n: N)(v: Visualization[N]) = v
      }

  }

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
