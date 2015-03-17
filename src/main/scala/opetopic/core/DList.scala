/**
  * DList.scala - Dimension Lists
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import Nat._

sealed trait DList[F[_ <: Nat, +_], N <: Nat, +A] {

  def :>>[B >: A](fn : F[N, B]) : DList[F, S[N], B] = 
    new :>>(this, fn)

  def dim : N

}

case class ||[F[_ <: Nat, +_]]() extends DList[F, _0, Nothing] { def dim = __0 }
case class :>>[F[_ <: Nat, +_], P <: Nat, A](tl : DList[F, P, A], hd : F[P, A]) extends DList[F, S[P], A] { def dim = S(tl.dim) }

object DList {

  def head[F[_ <: Nat, +_], N <: Nat, A](dl : DList[F, S[N], A]) : F[N, A] = 
    dl match {
      case (_ :>> hd) => hd
    }

  def tail[F[_ <: Nat, +_], N <: Nat, A](dl : DList[F, S[N], A]) : DList[F, N, A] = 
    dl match {
      case (tl :>> _) => tl
    }

}
