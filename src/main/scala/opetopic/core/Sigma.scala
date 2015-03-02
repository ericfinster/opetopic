/**
  * Sigma.scala - Simple Sigma-types over Nat
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

import Nats._

abstract class Sig[F[_ <: Nat]] {

  type Dim <: Nat

  val dim : Dim
  val value : F[Dim]

}

abstract class Sigma[F[_ <: Nat, _], A] {

  type Dim <: Nat

  val dim : Dim
  val value : F[Dim, A]

}

