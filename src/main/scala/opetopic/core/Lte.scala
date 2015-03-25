/**
  * Lte.scala - Less than or Equal Relation and corresponding utilities
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import Nat._

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

trait LteCaseSplit {

  type Out[M <: Nat, N <: Nat, D <: Nat]

  def caseZero[N <: Nat](n : N) : Out[_0, N, N]
  def caseSucc[M <: Nat, N <: Nat, D <: Nat](plte : Lte[M, N, D]) : Out[S[M], S[N], D]

  def apply[M <: Nat, N <: Nat, D <: Nat](lte : Lte[M, N, D]) : Out[M, N, D] = 
    lte match {
      case ZeroLte(n) => caseZero(n)
      case SuccLte(plte) => caseSucc(plte)
    }

}

object Lte {

  def lteSucc[M <: Nat, N <: Nat, D <: Nat](implicit lte : Lte[M, N, D]) : Lte[M, S[N], S[D]] = 
    lte match {
      case ZeroLte(n) => ZeroLte(S(n))
      case SuccLte(plte) => SuccLte(lteSucc(plte))
    }

  def lteRefl[N <: Nat](n : N) : Lte[N, N, _0] = 
    (new NatCaseSplit {

      type Out[M <: Nat] = Lte[M, M, _0]

      def caseZero : Lte[_0, _0, _0] = 
        ZeroLte(Z)

      def caseSucc[P <: Nat](p : P) : Lte[S[P], S[P], _0] = 
        SuccLte(lteRefl(p))

    })(n)

  def ltePred[M <: Nat, N <: Nat, D <: Nat](lte : Lte[S[M], N, D]) : Lte[M, N, S[D]] = 
    lte match {
      case SuccLte(plte) => lteSucc(plte)
    }

  def lteInvert[M <: Nat, N <: Nat, D <: Nat](lte : Lte[M, N, D]) : Lte[D, N, M] = 
    (new LteCaseSplit {

      type Out[M <: Nat, N <: Nat, D <: Nat] = Lte[D, N, M]

      def caseZero[N <: Nat](n : N) : Lte[N, N, _0] = 
        lteRefl(n)

      def caseSucc[M <: Nat, N <: Nat, D <: Nat](plte : Lte[M, N, D]) : Lte[D, S[N], S[M]] = 
        lteSucc(lteInvert(plte))

    })(lte)

  type Aux[M <: Nat, N <: Nat] = SigmaT[({ type L[D <: Nat] = Lte[M, N, D] })#L]

  def getLte[M <: Nat, N <: Nat](m : M, n : N) : Option[Aux[M, N]] = 
    (new NatCaseSplit {

      type Out[M0 <: Nat] = Option[Aux[M0, N]]

      def caseZero : Out[_0] = Some(SigmaT[({ type L[D <: Nat] = Lte[_0, N, D] })#L, N](n)(ZeroLte(n) : Lte[_0, N, N]))

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = 
        (new NatCaseSplit {

          type Out[N0 <: Nat] = Option[Aux[S[P], N0]]

          def caseZero : Out[_0] = None

          def caseSucc[Q <: Nat](q : Q) : Out[S[Q]] = 
            for {
              sigLte <- getLte(p, q)
            } yield SigmaT[({ type L[D <: Nat] = Lte[S[P], S[Q], D] })#L,sigLte.N](sigLte.n)(SuccLte(sigLte.value) : Lte[S[P], S[Q], sigLte.N])

        })(n)

    })(m)

}

