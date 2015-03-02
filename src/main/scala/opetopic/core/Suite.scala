/**
  * Suite.scala - Indexed Sequences of Various Types
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

import Nats._

sealed trait IncSeq[P[_ <: Nat, +_], N <: Nat, K <: Nat, +A] { 

  def length : K 

  def :>>[B >: A](head : P[N#Plus[K], B]) : IncSeq[P, N, S[K], B] = 
    new :>>(this, head)

}

case class IncNil[P[_ <: Nat, +_], N <: Nat, +A]() extends IncSeq[P, N, _0, Nothing] { def length = Z }
case class :>>[P[_ <: Nat, +_], N <: Nat, K <: Nat, +A](tail : IncSeq[P, N, K, A], head : P[N#Plus[K], A]) extends IncSeq[P, N, S[K], A] { def length = S(tail.length) }

sealed trait IncSeq0[P[_ <: Nat], N <: Nat, K <: Nat] {

  def length : K

  def >:>(head : P[N#Plus[K]]) : IncSeq0[P, N, S[K]] = 
    new >:>(this, head)

}

case class IncNil0[P[_ <: Nat], N <: Nat]() extends IncSeq0[P, N, _0] { def length = Z }
case class >:>[P[_ <: Nat], N <: Nat, K <: Nat](tail : IncSeq0[P, N, K], head : P[N#Plus[K]]) extends IncSeq0[P, N, S[K]] { def length = S(tail.length) }

object DimSeqs {

  type DimSeq0[P[_ <: Nat], N <: Nat] = IncSeq0[P, _0, S[N]]
  type DimSeq[P[_ <: Nat, +_], N <: Nat, +A] = IncSeq[P, _0, S[N], A]

  def head[P[_ <: Nat, +_], N <: Nat, A](seq : DimSeq[P, N, A]) : P[N, A] = 
    seq match {
      case t :>> h => h
    }

  def tail[P[_ <: Nat, +_], N <: Nat, A](seq : DimSeq[P, S[N], A]) : DimSeq[P, N, A] =
    seq match {
      case t :>> h => t
    }

  def dim[P[_ <: Nat, +_], N <: Nat, A](seq : DimSeq[P, N, A]) : N = 
    seq.length.pred

  def dim[P[_ <: Nat], N <: Nat](seq : DimSeq0[P, N]) : N = 
    seq.length.pred

}

// sealed trait DecSeq[P[_ <: Nat, +_], N <: Nat, K <: Nat, +A]
// case class DecNil[P[_ <: Nat, +_], N <: Nat, +A]() extends DecSeq[P, N, _0, A] 
// case class <<:[P[_ <: Nat, +_], N <: Nat, K <: Nat, +A](head : P[N, A], tail : DecSeq[P, S[N], K, A]) extends DecSeq[P, N, S[K], A] 

// object SeqTesting {

//   type F[N <: Nat, +A] = A
//   type TestSeq[N <: Nat, K <: Nat] = IncSeq[F, N, K, Int]
//   type TestDec[N <: Nat, K <: Nat] = DecSeq[F, N, K, Int]

//   def |>| : TestSeq[_0, _0] = IncNil[F, _0, Int]()
//   def |<| : TestDec[_3, _0] = DecNil[F, _3, Int]()

//   def test : TestSeq[_0, _4] = |>| :>> 4 :>> 17 :>> 1 :>> 289
//   def decseq : TestDec[_1, _2] = 23 <<: 3 <<: |<|

//   implicit def toDecSuccOps[P[_ <: Nat, +_], N <: Nat, K <: Nat, A](seq : DecSeq[P, S[N], K, A]) : DecSuccOps[P, N, K, A] = 
//     new DecSuccOps(seq)

//   class DecSuccOps[P[_ <: Nat, +_], N <: Nat, K <: Nat, A](seq : DecSeq[P, S[N], K, A]) {

//     def <<:[B >: A](head : P[N, A]) : DecSeq[P, N, S[K], B] = new <<:(head, seq)

//   }


// }
