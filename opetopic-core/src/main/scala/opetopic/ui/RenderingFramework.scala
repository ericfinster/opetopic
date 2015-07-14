/**
  * RenderingFramework.scala - An abstraction of required tools for rendering opetopic diagrams
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

abstract class RenderingFramework[U : Numeric] {

  val isNumeric = implicitly[Numeric[U]]
  import isNumeric._

  type ElementType

  type PathType <: ElementType
  type TextType <: ElementType
  type GroupType <: ElementType
  type RectangleType <: ElementType

  abstract class BoundedElement[E <: ElementType] {

    def element: E
    def bounds: BBox

  }

  abstract class BBox {

    def x: U
    def y: U

    def width: U = halfWidth * fromInt(2)
    def height: U = halfHeight * fromInt(2)

    def halfWidth: U
    def halfHeight: U

  }

  def group(elem: ElementType*) : GroupType
  def rect(x: U, y: U, width:U, height: U, r: U, strokeWidth: U) : RectangleType
  def path(d: String, strokeWidth: U) : PathType
  def text(str: String) : BoundedElement[TextType]

  def translate(el: ElementType, x: U, y: U) : ElementType

  trait Renderable[A, E <: ElementType] {

    def render(a: A) : BoundedElement[E]

  }

  // We should have another type class for color hints ...

  object Renderable {

    implicit object StringRenderable extends Renderable[String, TextType] {
      def render(str: String) = text(str)
    }

    implicit object IntRenderable extends Renderable[Int, TextType] {
      def render(i: Int) = text(i.toString)
    }

  }

}

