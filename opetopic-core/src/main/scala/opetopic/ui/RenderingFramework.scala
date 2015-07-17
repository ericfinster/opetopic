/**
  * RenderingFramework.scala - An abstraction of required tools for rendering opetopic diagrams
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._

abstract class RenderingFramework[U](implicit val isNumeric: Numeric[U], val isOrdered: Ordering[U]) {

  import isNumeric._

  type Element 

  type PathType <: Element
  type TextType <: Element
  type GroupType <: Element
  type RectangleType <: Element

  //============================================================================================
  // CANVAS API
  //

  def half(u: U) : U

  def group(elem: Element*) : GroupType
  def rect(x: U, y: U, width:U, height: U, r: U, stroke: String, strokeWidth: U, fill: String) : RectangleType
  def path(d: String, stroke: String, strokeWidth: U, fill: String) : PathType
  def text(str: String) : BoundedElement[TextType]

  case class Transform(
    val translateX : U,
    val translateY : U,
    val scaleX : U,
    val scaleY : U
  ) 

  def transform(el: Element, t: Transform) : Element
  def translate(el: Element, x: U, y: U) : Element
  def scale(el: Element, x: U, y: U) : Element

  //============================================================================================
  // BOUNDS
  //

  abstract class BoundedElement[E <: Element] {

    def element: E
    def bounds: BBox

  }

  case class BBox(val x: U, val y: U, val width: U, val height: U) {

    def halfWidth: U = half(width)
    def halfHeight: U = half(height)

    def dimString: String = 
      x.toString ++ " " ++ y.toString ++ " " ++
        width.toString ++ " " ++ height.toString

  }

  //============================================================================================
  // AFFIXABLE TYPECLASS
  //

  case class Decoration[E <: Element](
    val boundedElement : BoundedElement[E],
    val colorHint : String = "white"
  )

  trait Affixable[A, E <: Element] {

    def decoration(a: A) : Decoration[E]

  }

  object Affixable {

    implicit object StringAffixable extends Affixable[String, TextType] {
      def decoration(str: String) = Decoration(text(str))
    }

    implicit object IntAffixable extends Affixable[Int, TextType] {
      def decoration(i: Int) = Decoration(text(i.toString))
    }

  }

  trait AffixableFamily[A[_ <: Nat], E <: Element] {
    def apply[N <: Nat](n: N) : Affixable[A[N], E]
  }

  object AffixableFamily {

    implicit object ConstStringFamily extends AffixableFamily[ConstString, TextType] {
      def apply[N <: Nat](n: N) : Affixable[String, TextType] = 
        Affixable.StringAffixable
    }

    implicit object ConstIntFamily extends AffixableFamily[ConstInt, TextType] {
      def apply[N <: Nat](n: N) : Affixable[Int, TextType] = 
        Affixable.IntAffixable
    }

  }

  //============================================================================================
  // TEXT RENDERING
  //

  def renderTextGroup(str: String, font: ScalaSVGFont, stroke: String, strokeWidth: U, fill: String) : BoundedElement[GroupType] = {

    var advance : U = zero
    var ascent : U = zero
    var descent : U = zero

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

    new BoundedElement[GroupType] {

      val element = group(paths: _*)
      val bounds = BBox(zero, -ascent, advance, ascent + descent)

    }

  }


}

