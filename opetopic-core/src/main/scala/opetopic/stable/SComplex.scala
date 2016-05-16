/**
  * SComplex.scala - Stable Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Traverse
import scalaz.Applicative
import scalaz.syntax.traverse._
import scalaz.std.option._

trait ComplexTypes {

  type SComplex[+A] = Suite[SNesting[A]]
  type SCmplxZipper[+A] = Suite[SNstZipper[A]]

  implicit object ComplexTraverse extends Traverse[SComplex] {

    def traverseImpl[G[_], A, B](c: SComplex[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[SComplex[B]] =
      Traverse[Suite].traverse(c)(_.traverse(f))
    
  }

  implicit class SCmplxZipperOps[A](z: SCmplxZipper[A]) {

    //
    //  Focus information
    //

    def focus: SNesting[A] = 
      z.head.focus

    def focusDeriv: Option[SDeriv[A]] =
      focus match {
        case SDot(_) => None
        case SBox(_, cn) => Some(SDeriv(cn.asShell))
      }

    def focusSpine: Option[STree[A]] = 
      focus match {
        case SDot(a) => 
          for {
            tl <- z.tail
            d <- tl.focusDeriv
          } yield d.plug(a)
        case SBox(a, cn) => cn.spine
      }

    def focusCanopy: Option[STree[SAddr]] = 
      focus match {
        case SDot(_) => None
        case SBox(a, cn) => Some(cn.mapWithAddr({ case (_, addr) => addr }))
      }

    def focusUnit: Option[STree[SNesting[A]]] = 
      for {
        sp <- focusSpine
        res <- sp match {
          case SLeaf => 
            for {
              tl <- z.tail
              u <- tl.focusUnit
            } yield SNode(focus, u.asShell)
          case SNode(a, sh) => 
            for {
              extents <- sh.extents
            } yield SNode(focus, extents.asShell)
        }
      } yield res

    //
    //  Basic Zipper Ops
    // 

    def close: SComplex[A] = 
      Traverse[Suite].map(z)(_.close)

    def visit(dir: SDir): Option[SCmplxZipper[A]] = 
      (z, dir) match {
        case (||(hd), d) => 
          for {
            zp <- hd.visit(dir)
          } yield ||(zp)
        case (tl >> hd, SDir(Nil)) => 
          for {
            zp <- hd.visit(dir)
          } yield tl >> zp
        case (tl >> hd, SDir(d :: ds)) => 
          for {
            zp <- z.visit(SDir(ds))
            hz <- hd.sibling(d)
            sp <- zp.focusSpine
            res <- sp match {
              case SLeaf => zp.tail.map(_ >> hz)
              case SNode(a, sh) => 
                for {
                  extents <- sh.extents
                  addr <- extents.elementAt(d.dir)
                  tl <- zp.tail
                  ztl <- tl.seek(addr)
                } yield ztl >> hz
            }
          } yield res
      }

    def seek(addr: SAddr): Option[SCmplxZipper[A]] = 
      addr match {
        case Nil => Some(z)
        case d :: ds =>
          for {
            zp <- z.seek(ds)
            zzp <- zp.visit(d)
          } yield zzp
      }

    //
    //  Source Calculation
    //


  }

}
