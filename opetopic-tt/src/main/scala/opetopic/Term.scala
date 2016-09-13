/**
  * Term.scala - Abstract Terms for OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ott

sealed trait Term
case object Tt extends Term
case object Type extends Term
case object Unt extends Term
case class Var(i: Int) extends Term
case class Pi(u: Term, v: Term) extends Term
case class Lam(u: Term) extends Term
case class App(u: Term, v: Term) extends Term
case class Sig(u: Term, v: Term) extends Term
case class Pair(u: Term, v: Term) extends Term
case class Fst(u: Term) extends Term
case class Snd(u: Term) extends Term
