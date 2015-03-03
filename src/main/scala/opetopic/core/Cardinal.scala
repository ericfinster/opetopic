/**
  * Cardinal.scala - Opetopic Cardinal
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import scalaz.Leibniz._

import Nats._

object Cardinals {

  //============================================================================================
  // TREE SEQUENCES
  //

  trait TreeSeqRec[N <: Nat[N]] extends NatConsRec[AnyRef] {

    type OnZero[+A] = Tree[N, A]

    type OnSucc[P <: Nat[P], T[+_] <: AnyRef, +A] =
      Tree[N, P#ConsRec[AnyRef, TreeSeqRec[S[N]] , A]]

  }

  type TreeSeq[N <: Nat[N], K <: Nat[K], +A] = K#ConsRec[AnyRef, TreeSeqRec[N], A]

  //============================================================================================
  // CARDINALS
  //

  trait CardinalTreeRec extends NatConsRec[AnyRef] {

    type OnZero[+A] = Tree[Z, A]

    type OnSucc[P <: Nat[P], T[+_] <: AnyRef, +A] =
      T[Tree[S[P], A]]

  }

  type CardinalTree[N <: Nat[N], +A] = N#ConsRec[AnyRef, CardinalTreeRec, A]
  // type CardinalNesting[N <: Nat[N], +A] = CardinalTree[N, Nesting[N, A]]

  //============================================================================================
  // TYPE EQUALITY LEMMAS
  //

  def treeSeqAssoc[N <: Nat[N], M <: Nat[M], K <: Nat[K], D <: Nat[D], L <: Lte[K, M, D, L], A](
    implicit sum : Sum[N, K], lte : L
  ) : TreeSeq[N, S[M], A] === TreeSeq[N, K, TreeSeq[S[sum.Out], D, A]] = 
    (new LteCaseSplit {

      type Out[K0 <: Nat[K0], M0 <: Nat[M0], D0 <: Nat[D0]] = 
        TreeSeq[N, S[M0], A] === TreeSeq[N, K0, TreeSeq[S[sum.Out], D0, A]]


      def caseNZ[M0 <: Nat[M0]](m0 : M0) : Out[_0, M0, M0] = ???

      def caseSS[K0 <: Nat[K0], M0 <: Nat[M0], D0 <: Nat[D0], L0 <: Lte[K0, M0, D0, L0]](plte : L0) : Out[S[K0], S[M0], D0] = ???

  // def caseNZ[N <: Nat[N]](n : N) : Out[_0, N, N]
  // def caseSS[M <: Nat[M], N <: Nat[N], D <: Nat[D], L <: Lte[M, N, D, L]](plte : Lte[M, N, D, L]) : Out[S[M], S[N], D]

    })(lte)

  // def treeSeqAssoc[N <: Nat, M <: Nat, K <: Nat, D <: Nat, A](implicit lte : Lte[K, M, D]) 
  //     : TreeSeq[N, S[M], A] === TreeSeq[N, K, TreeSeq[S[K#Plus[N]], D, A]] = 
  //   (new LteSimpleMatch {

  //     type Out[X <: Nat, Y <: Nat, E <: Nat] = 
  //       TreeSeq[N, S[Y], A] === TreeSeq[N, X, TreeSeq[S[X#Plus[N]], E, A]]

  //     def caseZero[X <: Nat](x : X) : TreeSeq[N, S[X], A] === TreeSeq[N, _0, TreeSeq[S[N], X, A]] = refl

  //     def caseSucc[X <: Nat, Y <: Nat, E <: Nat](plte : Lte[X, Y, E])
  //         : TreeSeq[N, S[S[Y]], A] === TreeSeq[N, S[X], TreeSeq[S[S[X]#Plus[N]], E, A]] = {

  //       // In above, we have X = K, Y = M, N = N

  //       val step1 : Tree[N, Tree[S[N], TreeSeq[S[S[N]], Y, A]]] ===
  //                   Tree[N, TreeSeq[S[N], X, TreeSeq[S[X#Plus[S[N]]], E, A]]] = 
  //                     lift[Nothing, Nothing, Any, Any, 
  //                       ({ type L[+B] = Tree[N, B] })#L, 
  //                       Tree[S[N], TreeSeq[S[S[N]], Y, A]], 
  //                       TreeSeq[S[N], X, TreeSeq[S[X#Plus[S[N]]], E, A]]
  //                     ](treeSeqAssoc[S[N], Y, X, E, A](plte))

  //       val step2 : Tree[N, TreeSeq[S[N], X, TreeSeq[S[X#Plus[S[N]]], E, A]]] === 
  //                   Tree[N, TreeSeq[S[N], X, TreeSeq[S[S[X#Plus[N]]], E, A]]] = 
  //                     lift[Nothing, Nothing, Nat, Any,
  //                       ({ type L[P <: Nat] = Tree[N, TreeSeq[S[N], X, TreeSeq[S[P], E, A]]] })#L,
  //                       X#Plus[S[N]], S[X#Plus[N]]
  //                     ](plusSuccLemma[X, N](plte.lower))

  //       step1.andThen(step2)

  //     }

  //   })(lte)



}
