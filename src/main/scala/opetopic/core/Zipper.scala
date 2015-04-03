/**
  * Zipper.scala - Derivatives and Zippers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

trait ZipperExports {

  //============================================================================================
  // DERIVATIVES
  //

  trait DerivativeRec extends NatConsRec[Any] {

    type OnZero[+A] = Unit
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = 
      (Tree[Tree[A, S[P]], P], List[(A, T[Tree[A, S[P]]])])

  }

  type Derivative[+A, N <: Nat] = N#ConsRec[Any, DerivativeRec, A]

  type DerivSucc[+A, P <: Nat] = (Tree[Tree[A, S[P]], P], List[(A, Derivative[Tree[A, S[P]], P])])
  type DerivDblSucc[+A, P <: Nat] = (Tree[Tree[A, S[S[P]]], S[P]], List[(A, DerivSucc[Tree[A, S[S[P]]], P])])

  //============================================================================================
  // CONTEXTS
  //

  trait ContextRec extends NatConsRec[Any] {

    type OnZero[+A] = Unit
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = 
      List[(A, P#ConsRec[Any, DerivativeRec, Tree[A, S[P]]])]

  }

  type Context[+A, N <: Nat] = N#ConsRec[Any, ContextRec, A]

  type CntxtSucc[+A, P <: Nat] = List[(A, Derivative[Tree[A, S[P]], P])]
  type CntxtDblSucc[+A, P <: Nat] = List[(A, DerivSucc[Tree[A, S[S[P]]], P])]

  //============================================================================================
  // ZIPPERS
  //

  type Zipper[+A, N <: Nat] = (Tree[A, N], Context[A, N])

  type ZipperSucc[+A, P <: Nat] = (Tree[A, S[P]], CntxtSucc[A, P])
  type ZipperDblSucc[+A, P <: Nat] = (Tree[A, S[S[P]]], CntxtDblSucc[A, P])

}

trait TestTrait {

  def plug[A, N <: Nat](n: N)(deriv: Derivative[A, N], a: A) : Tree[A, N] = ???

//   def plug[N <: Nat, A](n : N)(deriv : Derivative[N, A], a : A) : Tree[N, A] = 
//     (new NatCaseSplit {

//       type Out[N <: Nat] = Derivative[N, A] => Tree[N, A]

//       def caseZero : Derivative[_0, A] => Tree[_0, A] = {
//         _ => Pt(a)
//       }

//       def caseSucc[P <: Nat](p : P) : Derivative[S[P], A] => Tree[S[P], A] = {
//         case (sh, cntxt) => close(S(p))(cntxt, Node(a, sh))
//       }

//     })(n)(deriv)


//   def close[N <: Nat, A](n : N)(cntxt : Context[N, A], tr : Tree[N, A]) : Tree[N, A] = 
//     (new NatCaseSplit {

//       type Out[N <: Nat] = (Context[N, A], Tree[N, A]) => Tree[N, A]

//       def caseZero : (Context[_0, A], Tree[_0, A]) => Tree[_0, A] = {
//         case (cntxt0, tr0) => tr0
//       }

//       def caseSucc[P <: Nat](p : P) : (Context[S[P], A], Tree[S[P], A]) => Tree[S[P], A] = {
//         case (Nil, trS) => trS
//         case ((a, d) :: cs, trS) => close(S(p))(cs, Node(a, plug(p)(d, trS)))
//       }

//     })(n)(cntxt, tr)

}
