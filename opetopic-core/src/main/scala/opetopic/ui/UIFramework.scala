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
  type RectangleType <: Element

  implicit val isNumeric: Numeric[Size]
  implicit val isOrdered: Ordering[Size]

  import isNumeric._

  //============================================================================================
  // CANVAS ELEMENTS
  //

  def group(elem: Element*) : GroupType
  def rect(x: Size, y: Size, width: Size, height: Size, r: Size, stroke: String, strokeWidth: Size, fill: String) : RectangleType
  def path(d: String, stroke: String, strokeWidth: Size, fill: String) : PathType
  def text(str: String) : BoundedElement[TextType]

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
    val x: Size,
    val y: Size,
    val width: Size,
    val height: Size
  ) {

    def dimString: String = 
      x.toString ++ " " ++ y.toString ++ " " ++
        width.toString ++ " " ++ height.toString

  }

  trait BoundedElement[+E <: Element] {
    def element: E
    def bounds: Bounds
  }

  object BoundedElement {

    def apply[E <: Element](el: E, bnds: Bounds) = 
      new BoundedElement[E] {
        val element = el
        val bounds = bnds
      }
  }

  //============================================================================================
  // AFFIXABLE TYPECLASS
  //

  case class Decoration[+E <: Element](
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

    implicit def optionAffixable[A, E <: Element](implicit r: Affixable[A, E]) : Affixable[Option[A], Element] = 
      new Affixable[Option[A], Element] {
        def decoration(opt: Option[A]) =
          opt match {
            case None => Decoration(text(" "))
            case Some(a) => r.decoration(a)
          }
      }

    implicit def polarityAffixable[A, E <: Element](implicit r: Affixable[A, E]) : Affixable[Polarity[A], Element] = 
      new Affixable[Polarity[A], Element] {
        def decoration(pol: Polarity[A]) = 
          pol match {
            case Positive() => Decoration(text("+"))
            case Negative() => Decoration(text("-"))
            case Neutral(a) => r.decoration(a)
          }
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

  def renderTextGroup(str: String, font: ScalaSVGFont, stroke: String, strokeWidth: Size, fill: String) : BoundedElement[GroupType] = {

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

    BoundedElement(
      group(paths: _*),
      Bounds(zero, -ascent, advance, ascent + descent)
    )

  }

}
