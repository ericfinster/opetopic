/**
  * Renderable.scala - The Renderable Type Class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._

trait Renderable[A] {
  def render(frmwk: UIFramework)(a: A) : frmwk.CellRendering
}

object Renderable {

  def apply[A](implicit r: Renderable[A]): Renderable[A] = r

  implicit object StringRenderable extends Renderable[String] {
    def render(f: UIFramework)(s: String): f.CellRendering = 
      f.CellRendering(f.text(s))
  }

  implicit object IntRenderable extends Renderable[Int] {
    def render(f: UIFramework)(i: Int): f.CellRendering = 
      f.CellRendering(f.text(i.toString))
  }

  implicit def optRenderable[A](implicit r: Renderable[A]): Renderable[Option[A]] = 
    new Renderable[Option[A]] {
      def render(f: UIFramework)(opt: Option[A]): f.CellRendering = 
        opt match {
          case Some(a) => r.render(f)(a)
          case None => {
            import f._
            import isNumeric._
            f.CellRendering(
              spacer(Bounds(fromInt(0), fromInt(0), fromInt(600), fromInt(600)))
            )
          }
        }
    }

  implicit def polarityRenderable[A](implicit r: Renderable[A]): Renderable[Polarity[A]] =
    new Renderable[Polarity[A]] {
      def render(f: UIFramework)(pol: Polarity[A]): f.CellRendering = 
        pol match {
          case Positive() => f.CellRendering(f.text("+"), PolarityColorSpec)
          case Negative() => f.CellRendering(f.text("-"), PolarityColorSpec)
          case Neutral(a) => r.render(f)(a)
        }
    }

  implicit object SAddrRenderable extends Renderable[SAddr] {
    def render(f: UIFramework)(addr: SAddr): f.CellRendering = {
      val trRenderer = new TreeRenderer[f.type](f)
      val be = trRenderer.renderAddr(addr).be
      f.CellRendering(be)
    }
  }

  implicit object FaceAddrRenderable extends Renderable[FaceAddr] {
    def render(f: UIFramework)(fa: FaceAddr): f.CellRendering = {
      SAddrRenderable.render(f)(fa.address)
    }
  }
  
  
}
