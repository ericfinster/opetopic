/**
  * Nat.scala - Type Level Natural Numbers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

sealed trait Nat[N <: Nat[N]] { this : N => 

  def caseSplit(sp : NatCaseSplit) : sp.Out[N]

  // type Plus[K <: Nat[K]] <: Nat[N#Plus[K]]

}

case class Z() extends Nat[Z] { 
  
  def caseSplit(sp : NatCaseSplit) : sp.Out[Z] = 
    sp.caseZero

  // type Plus[K <: Nat[K]] = K

}

case class S[P <: Nat[P]](p : P) extends Nat[S[P]] { 

  def caseSplit(sp : NatCaseSplit) : sp.Out[S[P]] = 
    sp.caseSucc(p)

  // type Plus[K <: Nat[K]] = S[P#Plus[K]]

}

trait NatCaseSplit {

  type Out[N <: Nat[N]]

  def caseZero : Out[Z]
  def caseSucc[P <: Nat[P]](p : P) : Out[S[P]]

  def apply[N <: Nat[N]](n : N) : Out[N] = 
    n.caseSplit(this)

}

object Nat {

}
