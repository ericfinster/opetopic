/**
  * Complex.scala - Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.syntax.monad._

import TypeDefs._

trait ComplexFunctions {

  class IndexedTypes[A[_ <: Nat]] {

    type IdxdNst[K <: Nat] = Nesting[A[K], K]
    type IdxdZip[K <: Nat] = NestingZipper[A[K], K]
    type IdxdCntxt[K <: Nat] = NestingContext[A[K], K]

    object ComplexZipper {

      def unapply[N <: Nat](suite : Suite[IdxdZip, S[N]]) : Option[(Suite[IdxdZip, N], IdxdZip[N])] =
        suite match {
          case tl >> hd => Some(tl, hd)
        }

      def apply() : Suite[IdxdZip, _0] = ???

    }

  }


  def complexToZipper[A[_ <: Nat], N <: Nat](c: Complex[A, N])(implicit types: IndexedTypes[A]) : ComplexZipper[A, N] = {

    import types._

    Suite.map[IdxdNst, IdxdZip, S[N]](c)(new ~~>[IdxdNst, IdxdZip] {
      def apply[K <: Nat](nst : IdxdNst[K]) : IdxdZip[K] = (nst, Nil)
    })

  }

  def seal[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N])(implicit types: IndexedTypes[A]) : Complex[A, N] = {

    import types._

    Suite.map[IdxdZip, IdxdNst, S[N]](z)(new ~~>[IdxdZip, IdxdNst] {
      def apply[K <: Nat](zp : IdxdZip[K]) : IdxdNst[K] = Nesting.closeNesting(zp._1.dim)(zp._2, zp._1)
    })

  }

  def focusOf[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N])(implicit types: IndexedTypes[A]) : Nesting[A[N], N] = {
    import types._
    Suite.head[IdxdZip, N](z)._1
  }

  def contextOf[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N])(implicit types: IndexedTypes[A]) : NestingContext[A[N], N] = {
    import types._
    Suite.head[IdxdZip, N](z)._2
  }

  def updateFocus[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N], nst: Nesting[A[N], N])(
    implicit types: IndexedTypes[A]
  ) : ComplexZipper[A, N] = {
    import types._
    z match {
      case ComplexZipper(tl, hd) => tl >> (nst, hd._2)
    }
  }

  def focusValue[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N])(implicit types: IndexedTypes[A]) : A[N] = {
    Nesting.baseValue(focusOf(z))
  }

  def focusDeriv[M[+_], A[_ <: Nat], N <: Nat](z : ComplexZipper[A, N])(
    implicit sm: ShapeMonad[M], types: IndexedTypes[A]
  ) : M[Derivative[A[S[N]], S[N]]] = 
    (new NatCaseSplit0 {

      import types._

      type Out[N <: Nat] = ComplexZipper[A, N] => M[Derivative[A[S[N]], S[N]]]

      def caseZero : Out[_0] = {
        case ComplexZipper(_, (Obj(a), _)) => sm.pure(Pt(Leaf(__1)), Nil)
        case ComplexZipper(_, (Box(a, cn), _)) => sm.pure(Tree.const(cn, Leaf(__1)), Nil)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case ComplexZipper(z, (Dot(a, d), cntxt)) => sm.failWith(new ShapeLookupError)
        case ComplexZipper(z, (Box(a, cn), cntxt)) => sm.pure(Tree.const(cn, Leaf(S(S(p)))), Nil)
      }

    })(z.length.pred)(z)

  def focusSpine[M[+_], A[_ <: Nat], N <: Nat](z : ComplexZipper[A, N])(
    implicit sm: ShapeMonad[M], types: IndexedTypes[A]
  ) : M[Tree[A[N], N]] = 
    (new NatCaseSplit0 {

      import types._

      type Out[N <: Nat] = ComplexZipper[A, N] => M[Tree[A[N], N]]

      def caseZero : Out[_0] = {
        case ComplexZipper(_, (Obj(a), _)) => sm.pure(Pt(a))
        case ComplexZipper(_, (Box(a, cn), _)) => Nesting.spineFromCanopy(cn)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case ComplexZipper(z, (Dot(a, d), cntxt)) => for { deriv <- focusDeriv(z) } yield ???
        case ComplexZipper(z, (Box(a, cn), cntxt)) => Nesting.spineFromCanopy(cn)
      }

    })(z.length.pred)(z)

  def focusCanopy[M[+_], A[_ <: Nat], N <: Nat](z : ComplexZipper[A, N])(
    implicit sm: ShapeMonad[M], types: IndexedTypes[A]
  ) : M[Tree[Address[N], N]] =
    (new NatCaseSplit0 {

      import types._

      type Out[N <: Nat] = ComplexZipper[A, N] => M[Tree[Address[N], N]]

      def caseZero : Out[_0] = {
        _ => sm.pure(Pt(()))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case ComplexZipper(z, (Dot(a, d), cntxt)) => 
          sm.failWith(new ShapeLookupError)
        case ComplexZipper(z, (Box(a, cn), cntxt)) => 
          sm.pure(Tree.mapWithAddress(cn)({ case (_, addr) => addr }))
      }

    })(z.length.pred)(z)

  def focusUnit[M[+_], A[_ <: Nat], N <: Nat](z : ComplexZipper[A, N])(
    implicit sm: ShapeMonad[M], types: IndexedTypes[A]
  ) : M[Tree[Nesting[A[N], N], N]] = 
    (new NatCaseSplit0 {

      import types._

      type Out[N <: Nat] = ComplexZipper[A, N] => M[Tree[Nesting[A[N], N], N]]

      def caseZero : Out[_0] = { z => 
        sm.pure(Pt(focusOf(z)))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = { z => 
        for {
          tr <- focusSpine(z)
          res <- (
            tr match {
              case Leaf(d) => 
                for { 
                  u <- focusUnit(Suite.tail[IdxdZip, S[P]](z)) 
                } yield Node(focusOf(z), Tree.const(u, Leaf(d)))
              case Node(a, sh) => 
                for {
                  extents <- Tree.shellExtents(sh)
                } yield Node(focusOf(z), Tree.const(extents, Leaf(S(p))))
            }
          )
        } yield res
      }

    })(z.length.pred)(z)


  def visitComplex[M[+_], A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N], dir: Address[N])(
    implicit sm: ShapeMonad[M], types: IndexedTypes[A]
  ) : M[ComplexZipper[A, N]] = 
    (new NatCaseSplit0 {

      import types._

      type Out[N <: Nat] = (ComplexZipper[A, N], Address[N]) => M[ComplexZipper[A, N]]

      def caseZero : Out[_0] = {
        case (ComplexZipper(_, nst), _) => 
          for {
            z0 <- Nesting.visitNesting(__0)(nst, ())
          } yield ComplexZipper() >> z0
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (c, Nil) => 
          for {
            z0 <- Nesting.visitNesting(S(p))(Suite.head[IdxdZip, S[P]](c), Nil)
          } yield Suite.tail[IdxdZip, S[P]](c) >> z0
        case (c, d :: ds) => 
          for {
            z0 <- visitComplex[M, A, S[P]](c, ds)
            z1 <- Nesting.sibling(p)(Suite.head[IdxdZip, S[P]](c), d)
            tr <- focusSpine(z0)
            res <- (
              tr match {
                case Leaf(_) => 
                  sm.pure(Suite.tail[IdxdZip, S[P]](z0) >> z1)
                case Node(a, sh) => 
                  for {
                    extents <- Tree.shellExtents(sh)
                    recAddr <- Tree.valueAt(extents, d)
                    tl <- seekComplex(Suite.tail[IdxdZip, S[P]](z0), recAddr)
                  } yield (tl >> z1)
              }
            )
          } yield res
      }

    })(z.length.pred)(z, dir)
 
  def seekComplex[M[+_], A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N], addr: Address[S[N]])(
    implicit sm: ShapeMonad[M], types: IndexedTypes[A]
  ) : M[ComplexZipper[A, N]] = 
    addr match {
      case Nil => sm.pure(z)
      case (d :: ds) => 
        for { 
          z0 <- seekComplex(z, ds) 
          z1 <- visitComplex(z0, d)
        } yield z1
    }

  abstract class SourceRoutines[M[+_]](implicit sm: ShapeMonad[M]) {

    import scalaz.StateT 
    import scalaz.MonadState 
    import scalaz.Applicative

    type SourceM[A[_ <: Nat], N <: Nat, R] = StateT[M, Complex[A, N], R]

    def liftS[A[_ <: Nat], N <: Nat, R](mr : M[R]) : SourceM[A, N, R] = 
      StateT((cmplx : Complex[A, N]) => for { r <- mr } yield (cmplx, r))

    def sourceAt[A[_ <: Nat], N <: Nat](c: Complex[A, N], addr: Address[S[N]])(
      implicit types: IndexedTypes[A]
    ): M[Complex[A, N]] =
      for {
        c0 <- restrictAt(c, addr)
        res <- contractAt(c0, Nil)
      } yield res

    def restrictAt[A[_ <: Nat], N <: Nat](c: Complex[A, N], addr: Address[S[N]])(
      implicit types: IndexedTypes[A]
    ) : M[Complex[A, N]] =
      for {
        z <- seekComplex(complexToZipper(c), addr)
        z0 <- restrictFocus(z)
      } yield seal(z)

    def contractAt[A[_ <: Nat], N <: Nat](c: Complex[A, N], addr: Address[S[N]])(
      implicit types: IndexedTypes[A]
    ) : M[Complex[A, N]] =
      for {
        z <- seekComplex(complexToZipper(c), addr)
        z0 <- contractFocus(z)
      } yield seal(z0)

    def restrictFocus[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N])(
      implicit types: IndexedTypes[A]
    ) : M[ComplexZipper[A, N]] = 
      (new NatCaseSplit0 {

        import types._

        type Out[N <: Nat] = ComplexZipper[A, N] => M[ComplexZipper[A, N]]

        def caseZero : Out[_0] = { z =>
          sm.pure(ComplexZipper() >> (focusOf(z), Nil))
        }

        def caseSucc[P <: Nat](p: P) : Out[S[P]] = { z =>
          for {
            tr <- focusSpine(z)
            tl <- restrictFocus(Suite.tail[IdxdZip, S[P]](z))
            c <- exciseLocal(Nil, tr).exec(seal(tl))
          } yield complexToZipper(c) >> (focusOf(z), Nil)

        }

      })(z.length.pred)(z)

    def contractFocus[A[_ <: Nat], N <: Nat](z:  ComplexZipper[A, N])(
      implicit types: IndexedTypes[A]
    ) : M[ComplexZipper[A, N]] = 
      (new NatCaseSplit0 {

        import types._

        type Out[N <: Nat] = ComplexZipper[A, N] => M[ComplexZipper[A, N]]

        def caseZero : Out[_0] = { z => 
          sm.pure(updateFocus(z, Obj(focusValue(z))))
        }

        def caseSucc[P <: Nat](p: P) : Out[S[P]] = { z => 
          for {
            tr <- focusSpine(z)
            tl <- compressFocus(Suite.tail[IdxdZip, S[P]](z), tr)
          } yield tl >> (Dot(focusValue(z), S(p)), contextOf(z))
        }

      })(z.length.pred)(z)

    def compressFocus[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N], tr: Tree[A[S[N]], S[N]])(
      implicit types: IndexedTypes[A]
    ) : M[ComplexZipper[A, N]] = 
      for {
        cn <- compressLocal(z, tr)
      } yield updateFocus(z, Box(focusValue(z), cn))

    def compressLocal[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N], tr: Tree[A[S[N]], S[N]])(
      implicit types: IndexedTypes[A]
    ) : M[Tree[Nesting[A[N], N], N]] =
      tr match {
        case Leaf(_) => focusUnit(z)
        case Node(a, sh) =>
          for {
            cn <- focusCanopy(z)
            toJn <- Tree.matchTraverse(cn, sh)({ 
              case (d, tr) => 
                for {
                  z0 <- visitComplex(z, d)
                  r <- compressLocal(z0, tr)
                } yield r
            })
            res <- Tree.join(toJn)
          } yield res
      }

    def exciseLocal[A[_ <: Nat], N <: Nat](addr: Address[S[N]], tr: Tree[A[S[N]], S[N]])(
      implicit types: IndexedTypes[A]
    ) : SourceM[A, N, Unit] = {

      type SrcM[R] = SourceM[A, N, R]
      type SrcS[S, R] = StateT[M, S, R]

      val MS = MonadState[SrcS, Complex[A, N]]
      import MS._

      tr match {
        case Leaf(_) =>
          for {
            complex <- get
            contractResult <- liftS(contractAt(complex, addr))
            _ <- put(contractResult)
          } yield ()
        case Node(a, sh) => 
          for {
           _ <- Tree.traverseWithAddress[SrcM, Tree[A[S[N]], S[N]], Unit, N](sh)({ 
             case (t, d) => exciseLocal(d :: addr, t)
           })(implicitly[Applicative[SrcM]])
          } yield ()
      }

    }

  }

}


// trait ComplexFunctions { self : ComplexImplicits => 

//   type Complex[N <: Nat, +A] = ConsSeq[Nesting, S[N], A]

//   //============================================================================================
//   // TRAVERSE
//   //

//   def traverseComplex[N <: Nat, G[_], A, B](cmplx : Complex[N, A])(f : A => G[B])(implicit apG : Applicative[G]) : G[Complex[N, B]] = 
//     (new NatCaseSplit {

//       import apG.{pure, ap, ap2} 

//       type Out[N <: Nat] = Complex[N, A] => G[Complex[N, B]]

//       def caseZero : Out[_0] = {
//           case _ >>> nst => ap(nst traverse f)(pure((n : Nesting[_0, B]) => CNil() >>> n))
//       }

//       def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
//           case (tl >>> hd) => {
//             ap2(tl traverse f, hd traverse f)(
//               pure((t : Complex[P, B], h : Nesting[S[P], B]) => t >>> h)
//             )
//           }
//       }

//     })(cmplx.dim)(cmplx)

//   //============================================================================================
//   // FOREACH
//   //

//   def foreach[N <: Nat, A](cmplx : Complex[N, A])(op : A => Unit) : Unit = 
//     cmplx.fold(new ConsFold[Nesting, A] {

//       type Out[N <: Nat] = Unit

//       def caseZero : Out[_0] = ()

//       def caseSucc[P <: Nat](p : P, nst : Nesting[P, A], u : Unit) : Unit = 
//         for { a <- nst } { op(a) }

//     })

//   //============================================================================================
//   // ZIP COMPLEX
//   //

//   def zipComplex[N <: Nat, A, B](cmplxA : Complex[N, A], cmplxB : Complex[N, B]) : Option[Complex[N, (A, B)]] =
//     (new NatCaseSplit {

//       type Out[N <: Nat] = (Complex[N, A], Complex[N, B]) => Option[Complex[N, (A, B)]]

//       def caseZero : Out[_0] = {
//         case (_ >>> nA , _ >>> nB) =>
//           for {
//             nAB <- nA matchWith nB
//           } yield CNil() >>> nAB
//       }

//       def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
//         case (tlA >>> hdA, tlB >>> hdB) =>
//           for {
//             tlAB <- zipComplex(tlA, tlB)
//             hdAB <- hdA matchWith hdB
//           } yield (tlAB >>> hdAB)
//       }

//     })(cmplxA.dim)(cmplxA, cmplxB)

//   //============================================================================================
//   // SOURCE ROUTINES
//   //

//   import ComplexZipper._


//   //============================================================================================
//   // COMULTIPLY
//   //

//   def comultiply[N <: Nat, A](cmplx : Complex[N, A]) : Option[Complex[N, Sigma[Complex, A]]] = 
//     (new NatCaseSplit {

//       type Out[N <: Nat] = Complex[N, A] => Option[Complex[N, Sigma[Complex, A]]]

//       def caseZero : Out[_0] = {
//         case _ >>> Obj(a) => Some(CNil() >>> Obj(CNil() >>> Obj(a)))
//         case _ >>> Box(a, Pt(nst)) =>
//           for {
//             _ >>> int <- caseZero(CNil() >>> nst)
//           } yield CNil() >>> Box(CNil() >>> Obj(a), Pt(int))
//       }

//       def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
//         case (tl >>> hd) => 
//           for {
//             // You should have a separate method for traversing *with* the address.
//             newHd <- hd.zipWithAddress traverse ({
//               case (_, addr) => for { src <- sourceAt(tl >>> hd, addr) } yield complexToSigma(src)
//             })
//             newTl <- comultiply(tl)
//           } yield newTl >>> newHd
//       }

//     })(cmplx.dim)(cmplx)


// }

// trait ComplexImplicits { self : ComplexFunctions => 

//   implicit def complexIsTraverse[N <: Nat] : Traverse[({ type L[+A] = ConsSeq[Nesting, S[N], A] })#L] = 
//     new Traverse[({ type L[+A] = ConsSeq[Nesting, S[N], A] })#L] {

//       override def map[A, B](cmplx : Complex[N, A])(f : A => B) : Complex[N, B] = 
//         mapComplex(cmplx)(f)

//       override def traverseImpl[G[_], A, B](cmplx : Complex[N, A])(f : A => G[B])(implicit isA : Applicative[G]) : G[Complex[N, B]] = 
//         traverseComplex(cmplx)(f)

//     }

//   import scalaz.syntax.FunctorOps
//   import scalaz.syntax.functor._

//   implicit def complexToFunctorOps[N <: Nat, A](cmplx : ConsSeq[Nesting, S[N], A]) : FunctorOps[({ type L[+A] = ConsSeq[Nesting, S[N], A] })#L, A] = 
//     ToFunctorOps[({ type L[+A] = ConsSeq[Nesting, S[N], A] })#L, A](cmplx)

//   import scalaz.syntax.TraverseOps
//   import scalaz.syntax.traverse._

//   implicit def complexToTraverseOps[N <: Nat, A](cmplx : ConsSeq[Nesting, S[N], A]) : TraverseOps[({ type L[+A] = ConsSeq[Nesting, S[N], A] })#L, A] = 
//     ToTraverseOps[({ type L[+A] = ConsSeq[Nesting, S[N], A] })#L, A](cmplx)


//   implicit def complexToSigma[M <: Nat, A](cmplx : Complex[M, A]) : Sigma[Complex, A] =
//     new Sigma[Complex, A] {
//       type N = M
//       val n = cmplx.dim
//       val value = cmplx
//     }

//   implicit def complexFromSigma[A](cmplx : Sigma[Complex, A]) : Complex[cmplx.N, A] = 
//     cmplx.value

//   class ComplexOps[N <: Nat, A](cmplx : Complex[N, A]) {

//     def dim : N = cmplx.length.pred

//     def matchWith[B](cmplxB : Complex[N, B]) : Option[Complex[N, (A, B)]] = 
//       zipComplex(cmplx, cmplxB)

//     def foreach(op : A => Unit) : Unit = 
//       Complex.foreach(cmplx)(op)

//   }

//   implicit def complexToComplexOps[N <: Nat, A](cmplx : Complex[N, A]) : ComplexOps[N, A] = 
//     new ComplexOps(cmplx)

//   implicit def complexSigmaToComplexOps[A](cmplx : Sigma[Complex, A]) : ComplexOps[cmplx.N, A] = 
//     new ComplexOps(cmplx.value)

// }

// object Complex extends ComplexFunctions 
//     with ComplexImplicits

