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
