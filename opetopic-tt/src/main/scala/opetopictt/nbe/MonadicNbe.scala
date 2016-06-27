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

  // The thing is, it feels really like a toy model of what you
  // want is just dependent type theory with a unit type, where
  // type expressions satisfy left and right unit laws with respect
  // to abstracting over Unit.

  // Let's flesh out the analogy: 

  // Types <~~~> constructors
  // Terms <~~~> places
  // Sigma <~~~> mu
  // Unit <~~~> eta

  // The reduction (a : A) * Unit ~~~> A is the right eta-rule
  // the reduction (u : Unit) * A ~~~> A is the left eta-rule
  // the reduction (a : A) * (b : B a) * C(a, b) <~~~> (p : ((a: A) * B a)) C(fst p, snd p) is mu? assoc?

  // But there is one signifigant difference: in the expressions I have in mind,
  // there is no analog of (P : (a : A) -> B a) -> C P.
  // That is, there are not "variables" of higher order.

  // How do we distinguish between these two cases?  It feels like we want 
  // Pi around as a type, but some other "first order" quantifier as well.

  // What would this sort of language look like in something like
  // do notation?

  sealed trait Term
  case object Cons extends Term
  case object One extends Term
  case class Var(i: Int) extends Term
  case class Lam(t: Term) extends Term
  case class Sig(t: Term, u: Term) extends Term
  case class Pair(t: Term, u: Term) extends Term
  case class Fst(t: Term) extends Term
  case class Snd(t: Term) extends Term

  type Tm = Int => Term

  sealed trait Dom
  case object ConsD extends Dom
  case object OneD extends Dom
  case class PairD(a: Dom, b: Dom) extends Dom
  case class FstD(a: Dom) extends Dom
  case class SndD(a: Dom) extends Dom
  case class SigD(a: Dom, f: Dom => Dom) extends Dom
  case class LamD(f: Dom => Dom) extends Dom
  case class NeD(tm: Tm) extends Dom

  type FId[A] = Free[Id, A]

  def zero: FId[Unit] = Return(())
  def succ(n: FId[Unit]): FId[Unit] = Join[Id, Unit](n)

  // sealed trait PExpr[+A] extends Expr[A]

  // sealed trait Expr[+A]
  // case class ESig[+A](id: String, ty: Expr[A], tm: Expr[A], a: A) extends Expr[A]
  // case class EOne[+A](a: A) extends Expr[A]
  // case class PVar[+A](id: String, a:A) extends Expr[A]

  // implicit object ExprFunctor extends Functor[Expr] {

  //   def map[A, B](f: A => B)(ea: Expr[A]): Expr[B] =
  //     ea match {
  //       case EOne(a) => EOne(f(a))
  //       case PVar(id, a) => PVar(id, f(a))
  //       case ESig(id, ty, tm, a) => ESig(id, map(f)(ty), map(f)(tm), f(a))
  //     }

  // }

  // type Prog[A] = Free[Expr, A]

  // def sig(id: String, ty: Expr[Unit], tm: Expr[Unit]): Prog[Unit] = 
  //   liftF[Expr, Unit](ESig(id, ty, tm, ()))

  // def variable(id: String): Prog[Unit] = 
  //   liftF[Expr, Unit](PVar(id, ()))

  // def one: Prog[Unit] = 
  //   liftF[Expr, Unit](EOne(()))


  implicit object ExprTokenizer extends Tokenizer[Expr] {

    def tokenize(e: Expr) : Token =
      e match {
        case EOne => Literal("η")
        case EVar(id) => Literal(id)
        // case ESig(id, ty, tm) => Phrase(Delim("(", Phrase(Literal(id), Literal(":"), ty.tokenize), ") *"), tm.tokenize)
        case ESig(id, ty, tm) => Phrase(Literal("μ"), ty.parenthesize, Delim("(", Phrase(Literal("\\" + id +"."), tm.parenthesize), ")"))
      }

  }

  implicit object PExprTokenizer extends Tokenizer[PExpr] {

    def tokenize(p: PExpr): Token = 
      p match {
        case PVar(id) => Literal(id)
        case PFst(p) => Phrase(Literal("fst"), p.tokenize)
        case PSnd(p) => Phrase(Literal("snd"), p.tokenize)
        case PPair(p, q) => Delim("(", Phrase(p.tokenize, Literal(","), q.tokenize), ")")
      }

  }

  sealed trait Expr { override def toString: String = this.pprint }
  case class ESig(id: String, ty: Expr, tm: Expr) extends Expr 
  case object EOne extends Expr
  case class EVar(id: String) extends Expr

  // Okay.  I think you need to take nbe seriously.
  // That is, try to write a reflection/reification pair

  sealed trait MExpr[+A]
  case class MMu[+A](e: M[Expr], a: A) extends MExpr[A]

  implicit object MExprFunctor extends Functor[MExpr] {
    def map[A, B](f: A => B)(fa: MExpr[A]): MExpr[B] = 
      fa match {
        case MMu(e, a) => MMu(e, f(a))
      }
  }

  type M[A] = Free[MExpr, A]

  def mu(e: Expr) : M[Expr] = 
    liftF(MMu(Return(e), e))

  def etaExpand(e: Expr) : M[Expr] = 
    liftF(MMu(Return(e), EOne))

  def up(e: Expr) : M[Expr] = 
    e match {
      case EOne => Return(EOne)
      case EVar(id) => etaExpand(EVar(id))  // Interesting! Eta expand all constants ...
      case ESig(id, ty, tm) => Join(MMu(up(ty), up(tm)))
    }

  // Now that all contants have been eta expanded, we can safely *ignore*
  // the trailing parts of our expanded expression.  Now, I want to "flatten"
  // it somehow using the monad multiplication ....

  // def eval(m: M[Expr], i: Int = 0) : Expr = 
  //   m match {
  //     case Return(e) => e
  //     case Join(MMu(e, k)) => {

  //       val a : M[Expr] = e
  //       val b : M[Expr] = k

  //       ESig("x" + i.toString, eval(e, i+1), EVar("Test"))
  //     }
  //   }

  // def down(m: M[String], i: Int = 0) : Expr = 
  //   m match {
  //     case Return(id) => EVar(id)
  //     case Join(MMu(tyN, rem)) => ESig("x" + i.toString, EVar(tyN), down(rem, i+1))
  //   }

  // val third: M[String] = 
  //   for {
  //     _ <- mu("A")
  //     _ <- mu("B")
  //     _ <- mu("X")
  //     _ <- mu("Y")
  //   } yield "P"

  // val fourth: M[Expr] = 
  //   for {
  //     _ <- mu("A")
  //   } yield EOne

  implicit class ExprOps(e: Expr) {

    def normalize: Expr = 
      e match {
        case ESig(id, ty, EOne) => ty.normalize
        case ESig(id, EOne, tm) => tm.normalize
        case ESig(i0, ESig(i1, ty, t1), t0) => ESig(i0, ty, ESig(i1, t1.normalize, t0.normalize).normalize).normalize
        case _ => e
      }

    // def nbe : Expr = down(Monad[M].join(up(e)))

  }

  sealed trait PExpr { override def toString: String = this.pprint }
  case class PVar(id: String) extends PExpr
  case class PFst(p: PExpr) extends PExpr
  case class PSnd(p: PExpr) extends PExpr
  case class PPair(p: PExpr, q: PExpr) extends PExpr

  val e0 = ESig("a", EVar("A"), EOne)  
  val e1 = ESig("b", ESig("d", EVar("D"), EVar("E")), EVar("B"))
  val e2 = ESig("c", e0, e1)
  val e3 = ESig("u", EOne, EVar("C")) 
  val e4 = ESig("d", e3, e1)
  val e5 = ESig("e", e2, ESig("f", EOne, e3))

  val m0: M[Expr] = 
    for {
      _ <- mu(e0)
      _ <- mu(EVar("B"))
      _ <- mu(e4)
    } yield EVar("D")

}


