/**
  * Core.scala - Package Object for Core Package
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds

import scalaz.Monad

package object core 
    extends NatExports
    with AddressExports {

  //============================================================================================
  // SHAPE MONAD
  //

  trait ShapeMonad[M[+_]] extends Monad[M] {

    def failWith[A](se : ShapeError) : M[A]

  }

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

