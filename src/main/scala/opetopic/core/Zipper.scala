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

  implicitly[Address[_0] =:= Unit]
  implicitly[Address[_1] =:= List[Unit]]
  implicitly[Address[_2] =:= List[List[Unit]]]

  //============================================================================================
  // DERIVATIVES
  //

  trait DerivativeRec extends NatConsRec[Any] {

    type OnZero[+A] = Unit
    type OnSucc[P <: Nat[P], T[+_] <: Any, +A] = (Tree[P, Tree[S[P], A]], List[(A, T[Tree[S[P], A]])])

  }

  type Derivative[N <: Nat[N], +A] = N#ConsRec[Any, DerivativeRec, A]

  type Deriv0[+A] = Unit
  type Deriv1[+A] = (Tree[_0, Tree[_1, A]], List[(A, Deriv0[Tree[_1, A]])])
  type Deriv2[+A] = (Tree[_1, Tree[_2, A]], List[(A, Deriv1[Tree[_2, A]])])
  type Deriv3[+A] = (Tree[_2, Tree[_3, A]], List[(A, Deriv2[Tree[_3, A]])])

  implicitly[Deriv1[Int] =:= Derivative[_1, Int]]
  implicitly[Deriv2[Int] =:= Derivative[_2, Int]]
  implicitly[Deriv3[Int] =:= Derivative[_3, Int]]

  type DTest0 = Derivative[_0, Int]
  type DTest1 = Derivative[_1, Int]
  // type DTest2 = Derivative[_2, Int]

  //============================================================================================
  // CONTEXTS
  //

  trait ContextRec extends NatConsRec[Any] {

    type OnZero[+A] = Unit
    type OnSucc[P <: Nat[P], T[+_] <: Any, +A] = List[(A, P#ConsRec[Any, DerivativeRec, Tree[S[P], A]])]

  }

  type Context[N <: Nat[N], +A] = N#ConsRec[Any, ContextRec, A]

  //============================================================================================
  // ZIPPERS
  //

  type Zipper[N <: Nat[N], A] = (Tree[N, A], Context[N, A])

}
