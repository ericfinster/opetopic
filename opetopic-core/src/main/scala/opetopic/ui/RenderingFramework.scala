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

  type Element

  // Now, we need some drawing primitives.

  abstract class BBox {

    def x: U
    def y: U

    def width: U = halfWidth * fromInt(2)
    def height: U = halfHeight * fromInt(2)

    def halfWidth: U
    def halfHeight: U

  }

  def group(elem: Element*) : Element
  def rect(x: U, y: U, width:U, height: U, r: U, strokeWidth: U) : Element
  def path(d: String, strokeWidth: U) : Element

  def translate(el: Element, x: U, y: U) : Element
  def boundedText(str: String) : (Element, BBox)

  trait Renderable[A] {

    def render(a: A) : (Element, BBox)

  }

  object Renderable {

    implicit object StringRenderable extends Renderable[String] {
      def render(str: String) = boundedText(str)
    }

    implicit object IntRenderable extends Renderable[Int] {
      def render(i: Int) = boundedText(i.toString)
    }

  }

}

