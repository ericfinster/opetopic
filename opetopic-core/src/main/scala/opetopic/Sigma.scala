/**
  * Sigma.scala - Sigma Type
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds

trait Sigma[T[_ <: Nat]] {

  type N <: Nat
  val n : N

  val value : T[N]

}

object Sigma {

  def apply[T[_ <: Nat], M <: Nat](m: M)(tm: T[M]) : Sigma[T] = 
    new Sigma[T] {
      type N = M
      val n = m
      val value = tm
    }

}

trait SuccSigma[T[_ <: Nat]] {

  type P <: Nat
  val p : P

  val value : T[S[P]]

}

object SuccSigma {

  def apply[T[_ <: Nat], Q <: Nat](q: Q)(sv: T[S[Q]]) : SuccSigma[T] = 
    new SuccSigma[T] {
      type P = Q
      val p = q
      val value = sv
    }

}
