/**
  * Term.scala - Abstract Terms for OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ott

import opetopic._

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

case class Obj(u: Term) extends Term
case class Cell(u: Term, frm: SComplex[Term]) extends Term
case class IsTgtU(e : Term) extends Term
case class IsSrcU(e : Term, addr : SAddr) extends Term
case class Refl(e : Term) extends Term
case class Drop(e : Term) extends Term
case class Comp(pd : STree[Term]) extends Term
case class Fill(e : Term) extends Term
case class LiftTgt(e : Term, ev : Term, c : Term, t : Term) extends Term
case class LiftSrc(e : Term, ev : Term, c : Term, t : Term) extends Term
case class FillTgt(e : Term, ev : Term, c : Term, t : Term) extends Term
case class FillSrc(e : Term, ev : Term, c : Term, t : Term) extends Term
case class DropIsTgt(e : Term) extends Term
case class FillIsTgt(pd : STree[Term]) extends Term
case class ShellIsTgt(e : Term, ev : Term, pd : STree[Term], tgt : Term) extends Term
case class FillTgtIsTgt(e : Term, ev : Term, c : Term, t : Term) extends Term
case class FillSrcIsTgt(e : Term, ev : Term, c : Term, t : Term) extends Term
case class FillTgtIsSrc(e : Term, ev : Term, c : Term, t : Term) extends Term
case class FillSrcIsSrc(e : Term, ev : Term, c : Term, t : Term) extends Term
