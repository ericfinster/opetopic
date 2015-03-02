/**
  * Lte.scala - Less than or Equal Relation and corresponding utilities
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import Nats._

sealed trait Lte[M <: Nat[M], N <: Nat[N], D <: Nat[D], L <: Lte[M, N, D, L]] { 
  
  def upper : N
  def lower : M
  def diff : D

  def caseSplit(split : LteCaseSplit) : split.Out[M, N, D]

}

case class ZLteN[N <: Nat[N]](n : N) extends Lte[_0, N, N, ZLteN[N]] {

  def upper : N = n
  def lower : _0 = Z()
  def diff : N = n

  def caseSplit(split : LteCaseSplit) : split.Out[_0, N, N] = 
    split.caseNZ(n)

}

case class SLteS[M <: Nat[M], N <: Nat[N], D <: Nat[D], L <: Lte[M, N, D, L]](plte : Lte[M, N, D, L]) extends Lte[S[M], S[N], D, SLteS[M, N, D, L]] {

  def upper : S[N] = S(plte.upper)
  def lower : S[M] = S(plte.lower)
  def diff : D = plte.diff

  def caseSplit(split : LteCaseSplit) : split.Out[S[M], S[N], D] = 
    split.caseSS(plte)

}

trait LteCaseSplit {

  type Out[M <: Nat[M], N <: Nat[N], D <: Nat[D]]

  def caseNZ[N <: Nat[N]](n : N) : Out[_0, N, N]
  def caseSS[M <: Nat[M], N <: Nat[N], D <: Nat[D], L <: Lte[M, N, D, L]](plte : Lte[M, N, D, L]) : Out[S[M], S[N], D]

  def apply[M <: Nat[M], N <: Nat[N], D <: Nat[D], L <: Lte[M, N, D, L]](lte : Lte[M, N, D, L]) : Out[M, N, D] = 
    lte.caseSplit(this)

}
