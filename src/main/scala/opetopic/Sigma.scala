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


