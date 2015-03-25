/**
  * Sigma.scala - Type and Constructor Sigmas
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

trait Sigma[F[_ <: Nat, +_], A] {

  type N <: Nat

  val n : N
  val value : F[N, A]

}

trait SigmaT[F[_ <: Nat]] {

  type N <: Nat

  val n : N
  val value : F[N]

}

object SigmaT {

  def apply[F[_ <: Nat], M <: Nat](m : M)(fm : F[M]) : SigmaT[F] = 
    new SigmaT[F] {
      type N = M
      val n = m
      val value = fm
    }

}
