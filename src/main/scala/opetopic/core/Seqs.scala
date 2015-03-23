/**
  * DList.scala - Dimension Lists
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

import Nat._

//============================================================================================
// INDEXED TYPE SEQUENCES
//

sealed trait TypeSeq[F[_ <: Nat], L <: Nat] {

  def >>(fl : F[L]) : TypeSeq[F, S[L]] = 
    new >>(this, fl)

  def length : L

}

case class TNil[F[_ <: Nat]]() extends TypeSeq[F, _0] { def length = Z }
case class >>[F[_ <: Nat], P <: Nat](tl : TypeSeq[F, P], hd : F[P]) extends TypeSeq[F, S[P]] { def length = S(tl.length) }

object TypeSeq {

  def head[F[_ <: Nat], N <: Nat](ts : TypeSeq[F, S[N]]) : F[N] = 
    ts match {
      case (_ >> hd) => hd
    }

  def tail[F[_ <: Nat], N <: Nat](ts : TypeSeq[F, S[N]]) : TypeSeq[F, N] = 
    ts match {
      case (tl >> _) => tl
    }

}

//============================================================================================
// INDEXED CONSTRUCTOR SEQUENCES
//

sealed trait ConsSeq[F[_ <: Nat, +_], L <: Nat, +A] {

  def >>>[B >: A](fn : F[L, B]) : ConsSeq[F, S[L], B] = 
    new >>>(this, fn)

  def length : L

}

case class CNil[F[_ <: Nat, +_]]() extends ConsSeq[F, _0, Nothing] { def length = Z }
case class >>>[F[_ <: Nat, +_], P <: Nat, A](tl : ConsSeq[F, P, A], hd : F[P, A]) extends ConsSeq[F, S[P], A] { def length = S(tl.length) }

trait ConsFold[F[_ <: Nat, +_], A] {

  type Out[N <: Nat]

  def caseZero : Out[_0]
  def caseSucc[P <: Nat](fp : F[P, A], ih : Out[P]) : Out[S[P]]

}

object ConsSeq {

  def head[F[_ <: Nat, +_], N <: Nat, A](cs : ConsSeq[F, S[N], A]) : F[N, A] = 
    cs match {
      case (_ >>> hd) => hd
    }

  def tail[F[_ <: Nat, +_], N <: Nat, A](cs : ConsSeq[F, S[N], A]) : ConsSeq[F, N, A] = 
    cs match {
      case (tl >>> _) => tl
    }

  def fold[F[_ <: Nat, +_], N <: Nat, A](cs : ConsSeq[F, N, A])(fld : ConsFold[F, A]) : fld.Out[N] = 
    cs match {
      case CNil() => fld.caseZero
      case tl >>> hd => fld.caseSucc(hd, fold(tl)(fld))
    }

  class ConsSuccOps[F[_ <: Nat, +_], P <: Nat, A](seq : ConsSeq[F, S[P], A]) {

    def head : F[P, A] = 
      ConsSeq.head(seq)

    def tail : ConsSeq[F, P, A] = 
      ConsSeq.tail(seq)

    def fold(fld : ConsFold[F, A]) : fld.Out[S[P]] = 
      ConsSeq.fold(seq)(fld)

  }

  implicit def toConsSuccOps[F[_ <: Nat, +_], P <: Nat, A](seq : ConsSeq[F, S[P], A]) : ConsSuccOps[F, P, A] = 
    new ConsSuccOps(seq)

}
