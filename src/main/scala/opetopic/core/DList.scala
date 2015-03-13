/**
  * DList.scala - Dimension Lists
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import Nat._

sealed trait DList[F[_ <: Nat], N <: Nat] {

  def :>>(fn : F[N]) : DList[F, S[N]] = 
    new :>>(this, fn)

}

case class ||[F[_ <: Nat]]() extends DList[F, _0]
case class :>>[F[_ <: Nat], P <: Nat](tl : DList[F, P], hd : F[P]) extends DList[F, S[P]]

object Test {

  import Zippers._

  type CardinalAddress[N <: Nat] = DList[Address, S[N]]

  val addr0 : CardinalAddress[_1] = ||[Address]() :>> (()) :>> Nil

}
