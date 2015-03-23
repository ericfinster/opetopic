/**
  * Complex.scala - Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

import Nat._
import Tree._
import Zippers._

trait ComplexFunctions { self : ComplexImplicits => 

  type Complex[N <: Nat, +A] = ConsSeq[Nesting, S[N], A]

  //============================================================================================
  // MAP
  //

  def mapComplex[N <: Nat, A, B](cmplx : Complex[N, A])(f : A => B) : Complex[N, B] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = Complex[N, A] => Complex[N, B]

      def caseZero : Out[_0] = {
        case (tl >>> nst) => CNil[Nesting]() >>> (nst map f)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (tl >>> nst) => (tl map f) >>> (nst map f)
      }

    })(cmplx.dim)(cmplx)

  //============================================================================================
  // TRAVERSE
  //

  def traverseComplex[N <: Nat, G[_], A, B](cmplx : Complex[N, A])(f : A => G[B])(implicit apG : Applicative[G]) : G[Complex[N, B]] = 
    (new NatCaseSplit {

      import apG.{pure, ap, ap2} 

      type Out[N <: Nat] = Complex[N, A] => G[Complex[N, B]]

      def caseZero : Out[_0] = {
          case _ >>> nst => ap(nst traverse f)(pure((n : Nesting[_0, B]) => CNil() >>> n))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
          case (tl >>> hd) => {
            ap2(tl traverse f, hd traverse f)(
              pure((t : Complex[P, B], h : Nesting[S[P], B]) => t >>> h)
            )
          }
      }

    })(cmplx.dim)(cmplx)

  //============================================================================================
  // ZIP COMPLEX
  //

  def zipComplex[N <: Nat, A, B](cmplxA : Complex[N, A], cmplxB : Complex[N, B]) : Option[Complex[N, (A, B)]] =
    (new NatCaseSplit {

      type Out[N <: Nat] = (Complex[N, A], Complex[N, B]) => Option[Complex[N, (A, B)]]

      def caseZero : Out[_0] = {
        case (_ >>> nA , _ >>> nB) =>
          for {
            nAB <- nA matchWith nB
          } yield CNil() >>> nAB
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (tlA >>> hdA, tlB >>> hdB) =>
          for {
            tlAB <- zipComplex(tlA, tlB)
            hdAB <- hdA matchWith hdB
          } yield (tlAB >>> hdAB)
      }

    })(cmplxA.dim)(cmplxA, cmplxB)

  //============================================================================================
  // SOURCE ROUTINES
  //

  import ComplexZipper._

  def sourceAt[N <: Nat, A](cmplx : Complex[N, A], addr : Address[S[N]]) : Option[Complex[N, A]] = 
    for {
      c0 <- restrictAt(cmplx, addr)
      c1 <- contractAt(c0, rootAddr(cmplx.length))
    } yield c1


  def restrictAt[N <: Nat, A](cmplx : Complex[N, A], addr : Address[S[N]]) : Option[Complex[N, A]] = {
    for {
      cz <- seekComplex(addr, fromComplex(cmplx))
      cz0 <- restrictFocus(cz)
    } yield seal(cz0)
  }

  def contractAt[N <: Nat, A](cmplx : Complex[N, A], addr : Address[S[N]]) : Option[Complex[N, A]] = {
    for {
      cz <- seekComplex(addr, fromComplex(cmplx))
      cz0 <- contractFocus(cz)
    } yield seal(cz0)
  }

  //============================================================================================
  // COMULTIPLY
  //

  def comultiply[N <: Nat, A](cmplx : Complex[N, A]) : Option[Complex[N, Sigma[Complex, A]]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = Complex[N, A] => Option[Complex[N, Sigma[Complex, A]]]

      def caseZero : Out[_0] = {
        case _ >>> Obj(a) => Some(CNil() >>> Obj(CNil() >>> Obj(a)))
        case _ >>> Box(a, Pt(nst)) =>
          for {
            _ >>> int <- caseZero(CNil() >>> nst)
          } yield CNil() >>> Box(CNil() >>> Obj(a), Pt(int))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (tl >>> hd) => 
          for {
            // You should have a separate method for traversing *with* the address.
            newHd <- hd.zipWithAddress traverse ({
              case (_, addr) => for { src <- sourceAt(tl >>> hd, addr) } yield complexToSigma(src)
            })
            newTl <- comultiply(tl)
          } yield newTl >>> newHd
      }

    })(cmplx.dim)(cmplx)

  //============================================================================================
  // SOURCE CALCULATION MONAD
  //

  type SourceM[N <: Nat, A, R] = StateT[Option, Complex[N, A], R]

  def liftS[N <: Nat, A, R](opt : Option[R]) : SourceM[N, A, R] =
    StateT((cmplx : Complex[N, A]) => opt map (r => (cmplx, r)))

  def exciseLocal[N <: Nat, A](addr : Address[S[N]], tr : Tree[S[N], A]) : SourceM[N, A, Unit] = {

    type SrcM[R] = SourceM[N, A, R]
    type SrcS[S, R] = StateT[Option, S, R]

    val MS = MonadState[SrcS, Complex[N, A]]
    import MS._

    tr match {
      case Leaf(_) =>
        for {
          complex <- get
          contractResult <- liftS(contractAt(complex, addr))
          _ <- put(contractResult)
        } yield ()
      case Node(a, sh) =>
        for {  // A bit ugly ....
          _ <- Tree.traverse[N, SrcM, (Address[N], Tree[S[N], A]), Unit](sh.zipWithAddress)({ 
            case (d, t) => exciseLocal(d :: addr : Address[S[N]], t) 
          })(implicitly[Applicative[SrcM]])
        } yield ()
    }

  }

}

trait ComplexImplicits { self : ComplexFunctions => 

  implicit def complexIsTraverse[N <: Nat] : Traverse[({ type L[+A] = ConsSeq[Nesting, S[N], A] })#L] = 
    new Traverse[({ type L[+A] = ConsSeq[Nesting, S[N], A] })#L] {

      override def map[A, B](cmplx : Complex[N, A])(f : A => B) : Complex[N, B] = 
        mapComplex(cmplx)(f)

      override def traverseImpl[G[_], A, B](cmplx : Complex[N, A])(f : A => G[B])(implicit isA : Applicative[G]) : G[Complex[N, B]] = 
        traverseComplex(cmplx)(f)

    }

  import scalaz.syntax.FunctorOps
  import scalaz.syntax.functor._

  implicit def complexToFunctorOps[N <: Nat, A](cmplx : ConsSeq[Nesting, S[N], A]) : FunctorOps[({ type L[+A] = ConsSeq[Nesting, S[N], A] })#L, A] = 
    ToFunctorOps[({ type L[+A] = ConsSeq[Nesting, S[N], A] })#L, A](cmplx)

  import scalaz.syntax.TraverseOps
  import scalaz.syntax.traverse._

  implicit def complexToTraverseOps[N <: Nat, A](cmplx : ConsSeq[Nesting, S[N], A]) : TraverseOps[({ type L[+A] = ConsSeq[Nesting, S[N], A] })#L, A] = 
    ToTraverseOps[({ type L[+A] = ConsSeq[Nesting, S[N], A] })#L, A](cmplx)


  implicit def complexToSigma[M <: Nat, A](cmplx : Complex[M, A]) : Sigma[Complex, A] =
    new Sigma[Complex, A] {
      type N = M
      val n = cmplx.dim
      val value = cmplx
    }

  implicit def complexFromSigma[A](cmplx : Sigma[Complex, A]) : Complex[cmplx.N, A] = 
    cmplx.value

  class ComplexOps[N <: Nat, A](cmplx : Complex[N, A]) {

    def dim : N = cmplx.length.pred

    def matchWith[B](cmplxB : Complex[N, B]) : Option[Complex[N, (A, B)]] = 
      zipComplex(cmplx, cmplxB)

  }

  implicit def complexToComplexOps[N <: Nat, A](cmplx : Complex[N, A]) : ComplexOps[N, A] = 
    new ComplexOps(cmplx)

  implicit def complexSigmaToComplexOps[A](cmplx : Sigma[Complex, A]) : ComplexOps[cmplx.N, A] = 
    new ComplexOps(cmplx.value)

}

object Complex extends ComplexFunctions 
    with ComplexImplicits

