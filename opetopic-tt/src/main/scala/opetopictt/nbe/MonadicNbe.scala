/**
  * MonadicNbe.scala - Looking for a monadic normalization by evaluation mechanism
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopictt.nbe

import opetopictt.pprint._
import Token._
import Tokenizer._

import Monad._

object MonadicNbe {

  def error(msg: String) = 
    throw new IllegalStateException(msg)

  sealed trait Term
  case object Idx extends Term                               // The type of indices
  case class Cons(i: Term) extends Term                      // The type of constructors
  case class Var(i: Int) extends Term                        // Variables
  case class Phi(c: Term, d: Term) extends Term              // Decorations in constructors
  case class Kap(t: Term) extends Term                       // Place abstraction
  case class Sel(u: Term, v: Term) extends Term              // Place selection
  case class Mu(c: Term, d: Term) extends Term               // Monad multiplication
  case class Eta(i: Term) extends Term                       // Monad unit
  case class Box(c: Term, d: Term) extends Term              // Box constructors
  case class Dot(i: Term) extends Term                       // Dot constructors

  type Tm = Int => Term

  sealed trait Monad
  case object Base extends Monad
  case class Slice(m: Monad) extends Monad

  // Uh, right.  We also need some kind of 
  // application for places, a recursor for
  // generating decorations of boxes, and what else?

  sealed trait Dom
  case object IdxD extends Dom
  case class PhiD(d: Dom, f: PDom => Dom) extends Dom
  case class KapD(f: PDom => Dom) extends Dom
  case class ConsD(i: Dom) extends Dom
  case class TypeOf(p: PDom) extends Dom
  case class NeD(tm: Tm) extends Dom

  sealed trait PDom

  case class NeP(tm: Tm) extends PDom

  def up(a: Dom, tm: Tm): Dom = 
    a match {
      case PhiD(u, f) => KapD(p => up(f(p), k => Sel(tm(k), down(u, TypeOf(p))(k))))
      case ConsD(i) => ???
      case _ => NeD(tm)
    }

  // Reify a domain element along a semantic type
  def down(a: Dom, b: Dom): Tm = ???


}


