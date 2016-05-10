/**
  * Renderable.scala - The Renderable Type Class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import opetopic.ui._

trait Renderable[A] {
  def render(frmwk: UIFramework)(a: A) : frmwk.BoundedElement
}

object Renderable {

  implicit object StringRenderable extends Renderable[String] {
    def render(f: UIFramework)(s: String): f.BoundedElement = 
      f.text(s)
  }

  implicit object IntRenderable extends Renderable[Int] {
    def render(f: UIFramework)(i: Int): f.BoundedElement = 
      f.text(i.toString)
  }

  implicit def optRenderable[A](implicit r: Renderable[A]): Renderable[Option[A]] = 
    new Renderable[Option[A]] {
      def render(f: UIFramework)(opt: Option[A]): f.BoundedElement = 
        opt match {
          case Some(a) => r.render(f)(a)
          case None => {
            import f._
            import isNumeric._
            spacer(Bounds(fromInt(0), fromInt(0), fromInt(600), fromInt(600)))
          }
        }
    }

}
