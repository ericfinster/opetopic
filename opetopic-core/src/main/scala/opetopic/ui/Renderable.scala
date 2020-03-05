/**
  * Renderable.scala - The Renderable Type Class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._

trait Renderable[A, -F <: UIFramework] {
  def render(frmwk: F)(a: A) : frmwk.CellRendering
}

object Renderable {

  def apply[A, F <: UIFramework](implicit r: Renderable[A, F]): Renderable[A, F] = r


  implicit def unitRenderable[F <: UIFramework]: Renderable[Unit, F] =
    new Renderable[Unit, F] {
      def render(f: F)(u: Unit): f.CellRendering = {
        import f._
        import isNumeric._
        CellRendering(
          spacer(Bounds(fromInt(0), fromInt(0), fromInt(600), fromInt(600)))
        )
      }
    }

  implicit def stringRenderable[F <: UIFramework]: Renderable[String, F] =
    new Renderable[String, F] {
      def render(f: F)(s: String): f.CellRendering =
        f.CellRendering(f.text(s))
    }

  implicit def intRenderable[F <: UIFramework]: Renderable[Int, F] =
    new Renderable[Int, F] {
      def render(f: F)(i: Int): f.CellRendering =
        f.CellRendering(f.text(i.toString))
    }

  implicit def optRenderable[A, F <: UIFramework](implicit r: Renderable[A, F]): Renderable[Option[A], F] = 
    new Renderable[Option[A], F] {
      def render(f: F)(opt: Option[A]): f.CellRendering = 
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

  implicit def polarityRenderable[A, F <: UIFramework](implicit r: Renderable[A, F]): Renderable[Polarity[A], F] =
    new Renderable[Polarity[A], F] {
      def render(f: F)(pol: Polarity[A]): f.CellRendering = 
        pol match {
          case Positive() => f.CellRendering(f.text("+"), PolarityColorSpec)
          case Negative() => f.CellRendering(f.text("-"), PolarityColorSpec)
          case Neutral(a) => r.render(f)(a)
        }
    }

  implicit def sAddrRenderable[F <: UIFramework]: Renderable[SAddr, F] =
    new Renderable[SAddr, F] {
      def render(f: F)(addr: SAddr): f.CellRendering = {
        val trRenderer = new TreeRenderer[f.type](f)
        val be = trRenderer.renderAddr(addr).be
        f.CellRendering(be)
      }
    }

  implicit def faceAddrRenderable[F <: UIFramework]: Renderable[FaceAddr, F] =
    new Renderable[FaceAddr, F] {
      def render(f: F)(fa: FaceAddr): f.CellRendering = {
        sAddrRenderable.render(f)(fa.address)
      }
    }
  
  
}
