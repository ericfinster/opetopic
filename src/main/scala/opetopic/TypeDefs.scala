/**
  * TypeDefs.scala - Type Definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds

object TypeDefs extends NatConstants {

  //============================================================================================
  // ADDRESSES
  //

  trait AddressRec extends NatTypeRec[Any] {

    type OnZero = Unit
    type OnSucc[P <: Nat, T <: Any] = List[T]

  }

  type Address[N <: Nat] = N#TypeRec[Any, AddressRec]

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

  type NestingDerivative[+A, N <: Nat] = 
    (Tree[Nesting[A, N], N], NestingContext[A, N])

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

  type NestingContext[+A, N <: Nat] = 
    List[(A, Derivative[Nesting[A, N], N])]

  //============================================================================================
  // ZIPPERS
  //

  type Zipper[+A, N <: Nat] = (Tree[A, N], Context[A, N])

  type ZipperSucc[+A, P <: Nat] = (Tree[A, S[P]], CntxtSucc[A, P])
  type ZipperDblSucc[+A, P <: Nat] = (Tree[A, S[S[P]]], CntxtDblSucc[A, P])

  type NestingZipper[+A, N <: Nat] = 
    (Nesting[A, N], NestingContext[A, N])

  type NestingZipperDblSucc[+A, P <: Nat] =
    (Nesting[A, S[S[P]]], List[(A, DerivDblSucc[Nesting[A, S[S[P]]], P])])

  //============================================================================================
  // COMPLEXES
  //

  type Complex[A[_ <: Nat], N <: Nat] = 
    Suite[({ type L[K <: Nat] = Nesting[A[K], K] })#L, S[N]]

  def |:|[A[_ <: Nat]] : Suite[({ type L[K <: Nat] = Nesting[A[K], K] })#L, _0] = 
    SNil[({ type L[K <: Nat] = Nesting[A[K], K] })#L]()

  type ComplexZipper[A[_ <: Nat], N <: Nat] = 
    Suite[({ type L[K <: Nat] = NestingZipper[A[K], K] })#L, S[N]]

  def |::|[A[_ <: Nat]] : Suite[({ type L[K <: Nat] = NestingZipper[A[K], K] })#L, _0] = 
    SNil[({ type L[K <: Nat] = NestingZipper[A[K], K] })#L]()

  //============================================================================================
  // TREE SEQUENCES
  //

  trait TreeSeqRec[N <: Nat] extends NatConsRec[AnyRef] {
    type OnZero[+A] = Tree[A, N]
    type OnSucc[P <: Nat, T[+_] <: AnyRef, +A] =
      Tree[P#ConsRec[AnyRef, TreeSeqRec[S[N]] , A], N]
  }

  type TreeSeq[+A, N <: Nat, K <: Nat] = K#ConsRec[AnyRef, TreeSeqRec[N], A]

  type TreeSeqSucc[+A, N <: Nat, P <: Nat] = 
    Tree[TreeSeq[A, S[N], P], N]
  type TreeSeqDblSucc[+A, N <: Nat, P <: Nat] = 
    Tree[Tree[TreeSeq[A, S[S[N]], P], S[N]], N]
  type TreeSeqTrplSucc[+A, N <: Nat, P <: Nat] = 
    Tree[Tree[Tree[TreeSeq[A, S[S[S[N]]], P], S[S[N]]], S[N]], N]
  type TreeSeqQuadSucc[+A, N <: Nat, P <: Nat] = 
    Tree[Tree[Tree[Tree[TreeSeq[A, S[S[S[S[N]]]], P], S[S[S[N]]]], S[S[N]]], S[N]], N]

  //============================================================================================
  // AUXILLARY TYPES
  //

  trait CardinalTreeRec extends NatConsRec[AnyRef] {
    type OnZero[+A] = Tree[A, _0]
    type OnSucc[P <: Nat, T[+_] <: AnyRef, +A] = T[Tree[A, S[P]]]
  }

  type CardinalTree[+A, N <: Nat] = N#ConsRec[AnyRef, CardinalTreeRec, A]
  type CardinalNesting[+A, N <: Nat] = CardinalTree[Nesting[A, N], N]

  type CardinalTreeSucc[+A, P <: Nat] = 
    CardinalTree[Tree[A, S[P]], P]
  type CardinalTreeDblSucc[+A, P <: Nat] =
    CardinalTree[Tree[Tree[A, S[S[P]]], S[P]], P]
  type CardinalTreeTrplSucc[+A, P <: Nat] =
    CardinalTree[Tree[Tree[Tree[A, S[S[S[P]]]], S[S[P]]], S[P]], P]
  type CardinalTreeQuadSucc[+A, P <: Nat] = 
    CardinalTree[Tree[Tree[Tree[Tree[A, S[S[S[S[P]]]]], S[S[S[P]]]], S[S[P]]], S[P]], P]

  type CardinalNestingSucc[+A, P <: Nat] = 
    CardinalTreeSucc[Nesting[A, S[P]], P]
  type CardinalNestingDblSucc[+A, P <: Nat] = 
    CardinalTreeDblSucc[Nesting[A, S[S[P]]], P]
  type CardinalNestingTrplSucc[+A, P <: Nat] = 
    CardinalTreeTrplSucc[Nesting[A, S[S[S[P]]]], P]
  type CardinalNestingQuadSucc[+A, P <: Nat] = 
    CardinalTreeQuadSucc[Nesting[A, S[S[S[S[P]]]]], P]

  type Cardinal[A[_ <: Nat], N <: Nat] = 
    Suite[({ type L[K <: Nat] = CardinalNesting[A[K], K] })#L, S[N]]

  //============================================================================================
  // CARDINAL ADDRESSES AND DERIVATIVES
  //

  type CardinalAddress[N <: Nat] = 
    Suite[Address, S[N]]

  trait CardinalDerivRec extends NatConsRec[Any] {
    type OnZero[+A] = Unit
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = 
      (T[Tree[A, S[P]]], DerivSucc[A, P])

  }

  type CardinalDerivative[+A, N <: Nat] = N#ConsRec[Any, CardinalDerivRec, A]

}
