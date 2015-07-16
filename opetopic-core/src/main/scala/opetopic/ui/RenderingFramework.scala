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

  type ElementType

  type PathType <: ElementType
  type TextType <: ElementType
  type GroupType <: ElementType
  type RectangleType <: ElementType

  //============================================================================================
  // CANVAS API
  //

  def half(u: U) : U

  def group(elem: ElementType*) : GroupType
  def rect(x: U, y: U, width:U, height: U, r: U, stroke: String, strokeWidth: U, fill: String) : RectangleType
  def path(d: String, strokeWidth: U) : PathType
  def text(str: String) : BoundedElement[TextType]

  def translate(el: ElementType, x: U, y: U) : ElementType

  //============================================================================================
  // BOUNDS
  //

  abstract class BoundedElement[E <: ElementType] {

    def element: E
    def bounds: BBox

  }

  abstract class BBox {

    def x: U
    def y: U

    def width: U 
    def height: U 

    def halfWidth: U = half(width)
    def halfHeight: U = half(height)

    def dimString: String = 
      x.toString ++ " " ++ y.toString ++ " " ++
        width.toString ++ " " ++ height.toString

  }

  //============================================================================================
  // AFFIXABLE TYPECLASS
  //

  case class Decoration[E <: ElementType](
    val boundedElement : BoundedElement[E],
    val colorHint : String = "white"
  )

  trait Affixable[A, E <: ElementType] {

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

  trait AffixableFamily[A[_ <: Nat], E <: ElementType] {
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

}

