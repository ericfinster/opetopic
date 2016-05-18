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

  object SComplex {

    import opetopic._

    def apply[A, N <: Nat](c: Complex[Lambda[`K <: Nat` => A], N]): SComplex[A] = {

      type AFam[K <: Nat] = A

      @natElim
      def stabilize[K <: Nat](k: K)(c: Complex[AFam, K]): SComplex[A] = {
        case (Z, Complex(_, hd)) => ||(SNesting(hd))
        case (S(p), Complex(tl, hd)) => stabilize(p)(tl) >> SNesting(hd)
      }

      stabilize(c.length.pred)(c)

    }

  }

  implicit object ComplexTraverse extends Traverse[SComplex] {

    def traverseImpl[G[_], A, B](c: SComplex[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[SComplex[B]] =
      Traverse[Suite].traverse(c)(_.traverse(f))
    
  }

  implicit class SComplexOps[A](c: SComplex[A]) {

    //
    //  Source Calculation
    //

    def truncateToDim(i: Int) = 
      c.take(i + 1)

    def sourceAt(addr: SAddr): Option[SComplex[A]] = 
      for {
        z <- SCmplxZipper(c).seek(addr)
        f <- z.focusFace
      } yield f

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

    // This does a bunch of extra work calculating
    // spines for derivatives which don't get used.
    // You should use laziness here to avoid all the
    // extra work.
    def focusDeriv[B]: Option[SDeriv[B]] =
      z match {
        case ||(_) => Some(SDeriv(SNode(SLeaf, SLeaf)))
        case tl >> hd =>
          hd.focus match {
            case SDot(_) => 
              for {
                tc <- tl.focusCanopy
              } yield SDeriv(tc.asShell)
            case SBox(_, cn) => 
              for {
                sp <- cn.spine
                d <- tl.focusDeriv[SNesting[A]]
                dsh <- tl.focus.canopyWithGuide(sp, d)
              } yield SDeriv(dsh.asShell)
          }
      }

    def focusCanopy: Option[STree[SNesting[A]]] = 
      z.focus match {
        case SDot(_) => None
        case SBox(_, cn) => Some(cn)
      }

    // The spine starting from the current focus
    def focusSpine: Option[STree[A]] =
      focus match {
        case SDot(a) => focusDeriv[A].map(_.plug(a))
        case SBox(a, cn) => cn.spine
      }

    def focusFace: Option[SComplex[A]] = 
      z match {
        case ||(hd) => Some(||(SDot(hd.focus.baseValue)))
        case tl >> hd => 
          for {
            sp <- focusSpine
            d <- focusDeriv[STree[A]]
            c <- tl.extract(sp)
            dd <- SCmplxZipper(c).focusDeriv[SNesting[A]]
            chd <- c.head.compressWith(d.plug(sp), dd)
          } yield c.withHead(chd) >> SDot(hd.focus.baseValue)
      }

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
            hz <- zp.head.sibling(d)
            sp <- zp.focusSpine
            res <- sp match {
              case SLeaf => zp.tail.map(_ >> hz)
              case SNode(a, sh) => 
                for {
                  extents <- sh.extents
                  addr <- extents.elementAt(d.dir)
                  ntl <- zp.tail
                  ztl <- ntl.seek(addr)
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
    //  Source Extraction
    //

    def extract[B](guide: STree[B]): Option[SComplex[A]] = 
      z match {
        case ||(hd) => 
          for {
            pr <- hd.focus.exciseWith(guide, SDeriv(SNode(SLeaf, SLeaf)))
          } yield ||(pr._1)
        case tl >> hd => 
          for {
            d <- focusDeriv[SNesting[A]]
            pr <- hd.focus.exciseWith(guide, d)
            (excised, boxTr) = pr
            (localSpine, compressor) = boxTr.treeSplit({
              case SDot(a) => ??? // An error ...
              case SBox(a, cn) => (a, cn)
            })
            sp <- compressor.join
            c <- tl.extract(sp)
            dd <- SCmplxZipper(c).focusDeriv[SNesting[A]]
            chd <- c.head.compressWith(compressor, dd)
          } yield c.withHead(chd) >> excised
      }

  }

}
