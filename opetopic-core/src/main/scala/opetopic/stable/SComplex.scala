/**
  * SComplex.scala - Stable Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.StateT
import scalaz.Traverse
import scalaz.MonadState
import scalaz.MonadTrans
import scalaz.Applicative
import scalaz.syntax.traverse._
import scalaz.syntax.monad._
import scalaz.std.option._

trait ComplexTypes {

  type SComplex[+A] = Suite[SNesting[A]]
  type SCmplxZipper[+A] = Suite[SNstZipper[A]]

  object SCmplxZipper {
    def apply[A](c: SComplex[A]): SCmplxZipper[A] = 
      Traverse[Suite].map(c)(SNstZipper(_))
  }

  implicit object ComplexTraverse extends Traverse[SComplex] {

    def traverseImpl[G[_], A, B](c: SComplex[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[SComplex[B]] =
      Traverse[Suite].traverse(c)(_.traverse(f))
    
  }

  implicit class SComplexOps[A](c: SComplex[A]) {

    //
    //  Source Calculation
    //

    def sourceAt(addr: SAddr): Option[SComplex[A]] = 
      for {
        cc <- restrictAt(addr)
        rc <- cc.contractAt(Nil)
      } yield rc

    def restrictAt(addr: SAddr): Option[SComplex[A]] = 
      for {
        z <- SCmplxZipper(c).seek(addr)
        zz <- z.restrictFocus
      } yield zz.close

    def contractAt(addr: SAddr): Option[SComplex[A]] = 
      for {
        z <- SCmplxZipper(c).seek(addr)
        zz <- z.contractFocus
      } yield zz.close

    def target: Option[SComplex[A]] = 
      c match {
        case ||(_) => None
        case tl >> _ => tl.sourceAt(Nil)
      }

  }

  implicit class SCmplxZipperOps[A](z: SCmplxZipper[A]) {

    //
    //  Focus information
    //

    def focus: SNesting[A] = 
      z.head.focus

    def withFocus(n: SNesting[A]): SCmplxZipper[A] = 
      z match {
        case ||(hd) => ||(hd.withFocus(n))
        case tl >> hd => tl >> hd.withFocus(n)
      }

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

    def restrictFocus: Option[SCmplxZipper[A]] = 
      z match {
        case ||(hd) => Some(||(SNstZipper(hd.focus)))
        case tl >> hd => 
          for {
            sp <- focusSpine
            ntl <- tl.restrictFocus
            c <- exciseLocal(sp, Nil).exec(ntl.close)
          } yield SCmplxZipper(c) >> SNstZipper(hd.focus)
      }

    def contractFocus: Option[SCmplxZipper[A]] = 
      z match {
        case ||(hd) => Some(withFocus(SDot(hd.focus.baseValue)))
        case tl >> hd => 
          for {
            sp <- focusSpine
            ntl <- tl.compressFocus(sp)
          } yield ntl >> SNstZipper(SDot(hd.focus.baseValue), hd.ctxt)
      }
      
    def compressFocus(tr: STree[A]): Option[SCmplxZipper[A]] = 
      for {
        cn <- compressLocal(tr)
      } yield withFocus(SBox(focus.baseValue, cn))

    def compressLocal(tr: STree[A]): Option[STree[SNesting[A]]] = 
      tr match {
        case SLeaf => focusUnit
        case SNode(a, sh) => 
          for {
            cn <- focusCanopy
            toJn <- cn.matchTraverse(sh)({
              case (d, t) => 
                for {
                  zz <- visit(SDir(d))
                  r <- zz.compressLocal(t)
                } yield r
            })
            res <- toJn.join
          } yield res
      }

    type SourceM[R] = StateT[Option, SComplex[A], R]

    def exciseLocal(tr: STree[A], addr: SAddr): SourceM[Unit] = {

      type SrcT[M[_], R] = StateT[M, SComplex[A], R]
      type SrcS[S, R] = StateT[Option, S, R]

      val MS = MonadState[SrcS, SComplex[A]]
      import MS._

      val MT = MonadTrans[SrcT]
      import MT._

      tr match {
        case SLeaf => 
          for {
            complex <- get
            res <- liftM(complex.contractAt(addr))
            _ <- put(res)
          } yield ()
        case SNode(a, sh) => 
          for {
            _ <- sh.traverseWithAddr[SourceM, Unit](
              (t, d) => exciseLocal(t, SDir(d) :: addr)
            )
          } yield ()
      }

    }

  }

}
