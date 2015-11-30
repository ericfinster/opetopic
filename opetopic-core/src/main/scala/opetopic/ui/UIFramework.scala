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
  type ViewportType <: Element
  type RectangleType <: Element

  implicit val isNumeric: Numeric[Size]
  implicit val isOrdered: Ordering[Size]

  import isNumeric._

  //============================================================================================
  // CANVAS ELEMENTS
  //

  def viewport(bounds: Bounds, elem: Element*) : ViewportType
  def group(elem: Element*) : GroupType
  def rect(x: Size, y: Size, width: Size, height: Size, r: Size, stroke: String, strokeWidth: Size, fill: String) : RectangleType
  def path(d: String, stroke: String, strokeWidth: Size, fill: String) : PathType
  def text(str: String) : BoundedElement[TextType]
  def toast(str: String) : Unit = ()

  def spacer(bnds: Bounds) : BoundedElement[Element] = 
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

  // Okay, we shouldn't use a color hint, we should use a class.
  // Then the idea is that we can define all this in css using all
  // of the semantic options available to us.

  case class Decoration[+E <: Element](
    val boundedElement : BoundedElement[E],
    val classString: String = "cell"
  )

  trait Affixable[A] {
    type ElementType <: Element
    def decoration(a: A) : Decoration[ElementType]
  }

  object Affixable {

    implicit object StringAffixable extends Affixable[String] {
      type ElementType = TextType
      def decoration(str: String) = Decoration(text(str))
    }

    implicit object IntAffixable extends Affixable[Int] {
      type ElementType = TextType
      def decoration(i: Int) = Decoration(text(i.toString))
    }

    implicit def optionAffixable[A](implicit bnds: Bounds, r: Affixable[A]) : Affixable[Option[A]] =
      new Affixable[Option[A]] {
        type ElementType = Element
        def decoration(opt: Option[A]) =
          opt match {
            case None => Decoration(spacer(bnds))
            case Some(a) => r.decoration(a)
          }
      }

    implicit def polarityAffixable[A](implicit r: Affixable[A]) : Affixable[Polarity[A]] =
      new Affixable[Polarity[A]] {
        type ElementType = Element
        def decoration(pol: Polarity[A]) =
          pol match {
            case Positive() => Decoration(text("+"))
            case Negative() => Decoration(text("-"))
            case Neutral(a) => r.decoration(a)
          }
      }

  }

  trait AffixableFamily[A[_ <: Nat]] {
    def apply[N <: Nat](n: N) : Affixable[A[N]]
  }

  object AffixableFamily {

    implicit def constAffixable[A](implicit a: Affixable[A]) : AffixableFamily[Lambda[`N <: Nat` => A]] =
      new AffixableFamily[Lambda[`N <: Nat` => A]] {
        def apply[N <: Nat](n: N) = a
      }

    // implicit object ConstStringFamily extends AffixableFamily[ConstString] {
    //   def apply[N <: Nat](n: N) : Affixable[String] =
    //     Affixable.StringAffixable
    // }

    // implicit object ConstIntFamily extends AffixableFamily[ConstInt] {
    //   def apply[N <: Nat](n: N) : Affixable[Int] =
    //     Affixable.IntAffixable
    // }

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
