/**
  * Lte.scala - Less than or Equal Relation and corresponding utilities
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import Nats._

sealed trait Lte[M <: Nat, N <: Nat, D <: Nat] { 
  
  def upper : N
  def lower : M
  def diff : D

}

case class ZeroLte[N <: Nat](n : N) extends Lte[_0, N, N] {

  def upper : N = n
  def lower : _0 = Z
  def diff : N = n

}

case class SuccLte[M <: Nat, N <: Nat, D <: Nat](plte : Lte[M, N, D]) extends Lte[S[M], S[N], D] {

  def upper : S[N] = S(plte.upper)
  def lower : S[M] = S(plte.lower)
  def diff : D = plte.diff

}

// This is really not the full elimination priciple, which it is definitely
// possible to implement.  You should fix that.
trait LteSimpleMatch {

  type Out[M <: Nat, N <: Nat, D <: Nat]

  def caseZero[N <: Nat](n : N) : Out[_0, N, N]
  def caseSucc[M <: Nat, N <: Nat, D <: Nat](plte : Lte[M, N, D]) : Out[S[M], S[N], D]

  def apply[M <: Nat, N <: Nat, D <: Nat](lte : Lte[M, N, D]) : Out[M, N, D] = 
    lte match {
      case ZeroLte(n) => caseZero(n)
      case SuccLte(plte) => caseSucc(plte)
    }

}

trait LteTwoParamCaseSplit {

  type In0[M <: Nat, N <: Nat, D <: Nat]
  type In1[M <: Nat, N <: Nat, D <: Nat]
  type Out[M <: Nat, N <: Nat, D <: Nat]

  trait LteDepMatch[F[_ <: Nat, _ <: Nat, _ <: Nat], M <: Nat, N <: Nat, D <: Nat] extends Dep[F[M, N, D]]
  case class LteZeroDepMatch[F[_ <: Nat, _ <: Nat, _ <: Nat], N <: Nat]() extends LteDepMatch[F, _0, N, N]
  case class LteSuccDepMatch[F[_ <: Nat, _ <: Nat, _ <: Nat], M <: Nat, N <: Nat, D <: Nat](plte : Lte[M, N, D]) extends LteDepMatch[F, S[M], S[N], D]

  type ParamPair[M <: Nat, N <: Nat, D <: Nat] = (In0[M, N, D], In1[M, N, D])

  def caseZeroLte[N <: Nat](n : N, in0 : In0[_0, N, N], in1 : In1[_0, N, N]) : Out[_0, N, N]
  def caseSuccLte[M <: Nat, N <: Nat, D <: Nat](plte : Lte[M, N, D], in0 : In0[S[M], S[N], D], in1 : In1[S[M], S[N], D]) : Out[S[M], S[N], D]

  def makeMatch[M <: Nat, N <: Nat, D <: Nat](lte : Lte[M, N, D]) : LteDepMatch[ParamPair, M, N, D] = 
    (new LteSimpleMatch {

      type Out[M0 <: Nat, N0 <: Nat, D0 <: Nat] = 
        LteDepMatch[ParamPair, M0, N0, D0]

      def caseZero[N0 <: Nat](n0 : N0) = LteZeroDepMatch[ParamPair, N0]()
      def caseSucc[M0 <: Nat, N0 <: Nat, D0 <: Nat](plte : Lte[M0, N0, D0]) =
        LteSuccDepMatch[ParamPair, M0, N0, D0](plte)

    })(lte)


  def apply[M <: Nat, N <: Nat, D <: Nat](lte : Lte[M, N, D], in0 : In0[M, N, D], in1: In1[M, N, D]) : Out[M, N, D] = {
    val pp : ParamPair[M, N, D] = (in0, in1)
    Pack(makeMatch(lte), pp) match {
      case Pack(LteZeroDepMatch(), (in0Z, in1Z)) => caseZeroLte(lte.upper, in0Z, in1Z)
      case Pack(LteSuccDepMatch(plte), (in0S, in1S)) => caseSuccLte(plte, in0S, in1S)
    }
  }

}

object Lte {

  def lteSucc[M <: Nat, N <: Nat, D <: Nat](implicit lte : Lte[M, N, D]) : Lte[M, S[N], S[D]] = 
    lte match {
      case ZeroLte(n) => ZeroLte(S(n))
      case SuccLte(plte) => SuccLte(lteSucc(plte))
    }

  def lteRefl[N <: Nat](n : N) : Lte[N, N, _0] = 
    (new NatElim {

      type Out[M <: Nat] = Lte[M, M, _0]

      def caseZero : Lte[_0, _0, _0] = ZeroLte(Z)
      def caseSucc[P <: Nat](p : P, ih : Out[P]) : Lte[S[P], S[P], _0] = SuccLte(ih)

    })(n)

  def ltePred[M <: Nat, N <: Nat, D <: Nat](lte : Lte[S[M], N, D]) : Lte[M, N, S[D]] = 
    lte match {
      case SuccLte(plte) => lteSucc(plte)
    }

}

