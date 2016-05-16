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

    // def sourceAt(addr: SAddr): Option[SComplex[A]] = 
    //   for {
    //     cc <- restrictAt(addr)
    //     _ = println("Restriction done")
    //     rc <- cc.contractAt(Nil)
    //     _ = println("Contraction done")
    //   } yield rc

    // def restrictAt(addr: SAddr): Option[SComplex[A]] = 
    //   for {
    //     z <- SCmplxZipper(c).seek(addr)
    //     _ = println("Found address")
    //     zz <- z.restrictFocus
    //   } yield zz.close

    // def contractAt(addr: SAddr): Option[SComplex[A]] = 
    //   for {
    //     z <- SCmplxZipper(c).seek(addr)
    //     _ = println("Found contraction address")
    //     zz <- z.contractFocus
    //     _ = println("Contracted at focus")
    //   } yield zz.close

    // def target: Option[SComplex[A]] = 
    //   c match {
    //     case ||(_) => None
    //     case tl >> _ => tl.sourceAt(Nil)
    //   }

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

    def focusDeriv[B]: Option[SDeriv[B]] =
      focus match {
        case SDot(_) => { println("derivative fail") ; None }
        case SBox(_, cn) => Some(SDeriv(cn.asShell))
      }

    def focusSpine: Option[STree[A]] =
      focus match {
        case SDot(a) =>
          for {
            tl <- z.tail
            d <- tl.focusDeriv[A]
          } yield d.plug(a)
        case SBox(a, cn) => cn.spine
      }

    def extract(guide: STree[A]): Option[SComplex[A]] = 
      z match {
        case ||(hd) => None
        case tl >> hd => 
          for {
            d <- focusDeriv[A]
            trpl <- hd.focus.exciseWith(guide, d)
            (newFcs, localSpine, globalSpine) = trpl
            c <- tl.extract(globalSpine)
          } yield ???
      }

    def focusFace: Option[SComplex[A]] = 
      z match {
        case ||(hd) => Some(||(SDot(hd.focus.baseValue)))
        case tl >> hd => 
          for {
            sp <- focusSpine
            c <- extract(sp)
          } yield ??? //c.withHead(c.head.compressWith(???))
      }

  //   def focusCanopy: Option[STree[SAddr]] = 
  //     focus match {
  //       case SDot(_) => { println("canopy fail") ; None }
  //       case SBox(a, cn) => Some(cn.mapWithAddr[SAddr]((_, addr) => addr))
  //     }

  //   // @natElim
  //   // def focusUnit[A[_ <: Nat], N <: Nat](n: N)(z: ComplexZipper[A, N]) : ShapeM[Tree[Nesting[A[N], N], N]] = {
  //   //   case (Z, z) => succeed(Pt(focusOf(z)))
  //   //   case (S(p), z) =>

  //   def focusUnit: Option[STree[SNesting[A]]] = 
  //     z match {
  //       case ||(hd) => Some(STree.obj(hd.focus))
  //       case _ => 
  //         for {
  //           sp <- focusSpine
  //           res <- sp match {
  //             case SLeaf =>
  //               for {
  //                 tl <- z.tail
  //                 u <- tl.focusUnit
  //               } yield SNode(focus, u.asShell)
  //             case SNode(a, sh) =>
  //               for {
  //                 extents <- sh.extents
  //               } yield SNode(focus, extents.asShell)
  //           }
  //         } yield res
  //     }


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

  //   //
  //   //  Source Calculation
  //   //

  //   def restrictFocus: Option[SCmplxZipper[A]] = 
  //     z match {
  //       case ||(hd) => {
  //         println("restricted head")
  //         Some(||(SNstZipper(hd.focus)))
  //       }
  //       case tl >> hd => 
  //         for {
  //           sp <- focusSpine
  //           _ = println("Got the spine")
  //           ntl <- tl.restrictFocus
  //           _ = println("Restricted tail")
  //           c <- exciseLocal(sp, Nil).exec(ntl.close)
  //           _ = println("Finished excistion")
  //         } yield SCmplxZipper(c) >> SNstZipper(hd.focus)
  //     }

  //   def contractFocus: Option[SCmplxZipper[A]] = 
  //     z match {
  //       case ||(hd) => {
  //         println("Contracted head focus")
  //         Some(withFocus(SDot(hd.focus.baseValue)))
  //       }
  //       case tl >> hd => 
  //         for {
  //           sp <- focusSpine
  //           _ = println("Got contraction spine")
  //           ntl <- tl.compressFocus(sp)
  //           _ = println("Compressed focus")
  //         } yield ntl >> SNstZipper(SDot(hd.focus.baseValue), hd.ctxt)
  //     }
      
  //   def compressFocus(tr: STree[A]): Option[SCmplxZipper[A]] = 
  //     for {
  //       cn <- compressLocal(tr)
  //     } yield withFocus(SBox(focus.baseValue, cn))

  //   def compressLocal(tr: STree[A]): Option[STree[SNesting[A]]] = 
  //     tr match {
  //       case SLeaf => {
  //         println("Compressing a leaf")
  //         for {
  //           u <- focusUnit
  //           _ = println("Got the unit")
  //         } yield u
  //       }
  //       case SNode(a, sh) => 
  //         for {
  //           cn <- focusCanopy
  //           _ = println("Canopy is: " + cn.toString)
  //           _ = println("Shell is: " + sh.toString)
  //           toJn <- cn.matchTraverse(sh)({
  //             case (d, t) => 
  //               for {
  //                 zz <- visit(SDir(d))
  //                 _ = println("Visit ok")
  //                 r <- zz.compressLocal(t)
  //                 _ = println("Compression ok")
  //               } yield r
  //           })
  //           _ = println("finished match")
  //           res <- toJn.join
  //           _ = println("finished join")
  //         } yield res
  //     }

  //   type SourceM[R] = StateT[Option, SComplex[A], R]

  //   def exciseLocal(tr: STree[A], addr: SAddr): SourceM[Unit] = {

  //     type SrcT[M[_], R] = StateT[M, SComplex[A], R]
  //     type SrcS[S, R] = StateT[Option, S, R]

  //     val MS = MonadState[SrcS, SComplex[A]]
  //     import MS._

  //     val MT = MonadTrans[SrcT]
  //     import MT._

  //     tr match {
  //       case SLeaf => 
  //         for {
  //           complex <- get
  //           res <- liftM(complex.contractAt(addr))
  //           _ <- put(res)
  //         } yield ()
  //       case SNode(a, sh) => 
  //         for {
  //           _ <- sh.traverseWithAddr[SourceM, Unit](
  //             (t, d) => exciseLocal(t, SDir(d) :: addr)
  //           )
  //         } yield ()
  //     }

  //   }

  }

}
