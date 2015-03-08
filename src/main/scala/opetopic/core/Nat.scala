/**
  * Nat.scala - Type level natural numbers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

sealed trait Nat
case object Z extends Nat
case class S[P <: Nat](p : P) extends Nat

trait NatCaseSplit {

  type Out[N <: Nat]

  def caseZero : Out[Z.type]
  def caseSucc[P <: Nat](p: P) : Out[S[P]]

}

trait NatFunctions {

  def caseSplit[N <: Nat](n: N)(sp: NatCaseSplit) : sp.Out[N] = 
    n match {
      case Z => sp.caseZero.asInstanceOf[sp.Out[N]]
      case S(p) => sp.caseSucc(p).asInstanceOf[sp.Out[N]]
    }

}

trait NatImplicits { self : NatFunctions =>

  implicit class NatOps[N <: Nat](n : N) {

    def caseSplit(sp: NatCaseSplit) : sp.Out[N] = 
      self.caseSplit(n)(sp)

  }

}

trait NatConstants {

  type _0 = Z.type
  type _1 = S[_0]
  type _2 = S[_1]
  type _3 = S[_2]
  type _4 = S[_3]
  type _5 = S[_4]
  type _6 = S[_5]
  type _7 = S[_6]
  type _8 = S[_7]
  type _9 = S[_8]

  val __0 = Z
  val __1 = S(__0)
  val __2 = S(__1)
  val __3 = S(__2)
  val __4 = S(__3)
  val __5 = S(__4)
  val __6 = S(__5)
  val __7 = S(__6)
  val __8 = S(__7)
  val __9 = S(__8)

}

object Nat extends NatFunctions 
    with NatImplicits 
    with NatConstants


