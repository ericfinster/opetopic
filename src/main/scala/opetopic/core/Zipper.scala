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

}
