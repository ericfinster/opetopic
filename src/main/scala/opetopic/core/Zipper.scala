/**
  * Zipper.scala - Higher dimensional zippers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import Nats._

object Zippers {

  //============================================================================================
  // ADDRESSES
  //

  trait AddressRec extends NatTypeRec[Any] {

    type OnZero = Unit
    type OnSucc[P <: Nat[P], T <: Any] = List[T]

  }

  type Address[N <: Nat[N]] = N#TypeRec[Any, AddressRec]

  //============================================================================================
  // DERIVATIVES
  //

  trait DerivativeRec extends NatConsRec[Any] {

    type OnZero[+A] = Unit
    type OnSucc[P <: Nat[P], T[+_] <: Any, +A] = 
      (Tree[P, Tree[S[P], A]], List[(A, T[Tree[S[P], A]])])

  }

  type Derivative[N <: Nat[N], +A] = N#ConsRec[Any, DerivativeRec, A]

  type DerivSucc[P <: Nat[P], +A] = (Tree[P, Tree[S[P], A]], List[(A, Derivative[P, Tree[S[P], A]])])
  type DerivDblSucc[P <: Nat[P], +A] = (Tree[S[P], Tree[S[S[P]], A]], List[(A, DerivSucc[P, Tree[S[S[P]], A]])])

  def plug[N <: Nat[N], A](n : N)(deriv : Derivative[N, A], a : A) : Tree[N, A] = 
    (new NatCaseSplit {

      type Out[N <: Nat[N]] = Derivative[N, A] => Tree[N, A]

      def caseZero : Derivative[_0, A] => Tree[_0, A] = {
        _ => Pt(a)
      }

      def caseSucc[P <: Nat[P]](p : P) : Derivative[S[P], A] => Tree[S[P], A] = {
        case (sh, cntxt) => close(S(p))(cntxt, Node(a, sh))
      }

    })(n)(deriv)

  //============================================================================================
  // CONTEXTS
  //

  trait ContextRec extends NatConsRec[Any] {

    type OnZero[+A] = Unit
    type OnSucc[P <: Nat[P], T[+_] <: Any, +A] = 
      List[(A, P#ConsRec[Any, DerivativeRec, Tree[S[P], A]])]

  }

  type Context[N <: Nat[N], +A] = N#ConsRec[Any, ContextRec, A]

  type CntxtSucc[P <: Nat[P], +A] = List[(A, Derivative[P, Tree[S[P], A]])]
  type CntxtDblSucc[P <: Nat[P], +A] = List[(A, DerivSucc[P, Tree[S[S[P]], A]])]

  def close[N <: Nat[N], A](n : N)(cntxt : Context[N, A], tr : Tree[N, A]) : Tree[N, A] = 
    (new NatCaseSplit {

      type Out[N <: Nat[N]] = (Context[N, A], Tree[N, A]) => Tree[N, A]

      def caseZero : (Context[_0, A], Tree[_0, A]) => Tree[_0, A] = {
        case (cntxt0, tr0) => tr0
      }

      def caseSucc[P <: Nat[P]](p : P) : (Context[S[P], A], Tree[S[P], A]) => Tree[S[P], A] = {
        case (Nil, trS) => trS
        case ((a, d) :: cs, trS) => close(S(p))(cs, Node(a, plug(p)(d, trS)))
      }

    })(n)(cntxt, tr)

  //============================================================================================
  // ZIPPERS
  //

  type Zipper[N <: Nat[N], A] = (Tree[N, A], Context[N, A])

  type ZipperSucc[P <: Nat[P], A] = (Tree[S[P], A], CntxtSucc[P, A])
  type ZipperDblSucc[P <: Nat[P], A] = (Tree[S[S[P]], A], CntxtDblSucc[P, A])

  def visit[N <: Nat[N], A](n : N)(zp : Zipper[S[N], A], dir : Address[N]) : Option[Zipper[S[N], A]] = 
    (new NatCaseSplit {

      type Out[N <: Nat[N]] = (Zipper[S[N], A], Address[N]) => Option[Zipper[S[N], A]]

      def caseZero : (Zipper[_1, A], Address[_0]) => Option[Zipper[_1, A]] = {
        case ((Leaf(_), _), ()) => None
        case ((Node(hd, Pt(tl)), c), ()) => Some((tl, (hd, ()) :: c))
      }

      def caseSucc[P <: Nat[P]](p : P) : (ZipperDblSucc[P, A], Address[S[P]]) => Option[ZipperDblSucc[P, A]] = {
        case ((Leaf(_), _), _) => None
        case ((Node(a, sh), c), d) => 
          for {
            shZp <- seek[S[P], Tree[S[S[P]], A]]((sh, Nil), d)
            res <- (
              shZp match {
                case (Leaf(_), _) => None
                case (Node(tr, hsh), c0) => Some((tr, (a, (hsh, c0)) :: c))
              }
            )
          } yield res
      }

    })(n)(zp, dir)

  def seek[N <: Nat[N], A](zp : Zipper[N, A], addr : Address[N]) : Option[Zipper[N, A]] = 
    (new NatCaseSplit {

      type Out[N <: Nat[N]] = (Zipper[N, A], Address[N]) => Option[Zipper[N, A]]

      def caseZero : (Zipper[_0, A], Address[_0]) => Option[Zipper[_0, A]] = {
        case (zp0, ()) => Some(zp0)
      }

      def caseSucc[P <: Nat[P]](p : P) : (Zipper[S[P], A], Address[S[P]]) => Option[Zipper[S[P], A]] = {
        case (zpS, Nil) => Some(zpS)
        case (zpS, d :: ds) => 
          for {
            zpM <- caseSucc(p)(zpS, ds)
            zpF <- visit(p)(zpM, d)
          } yield zpF
      }

    })(zp._1.dim)(zp, addr)

}
