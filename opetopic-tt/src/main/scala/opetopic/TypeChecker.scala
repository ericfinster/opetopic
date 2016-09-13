/**
  * TypeChecker.scala - A TypeChecker for OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ott

import opetopic._
import mtl._

import OttSyntax._

object TypeChecker {

  //
  //  The Semantic Domain
  //

  sealed trait Dom
  case object TtD extends Dom
  case object TypeD extends Dom
  case object UnitD extends Dom
  case class VarD(i: Int) extends Dom
  case class PiD(a: Dom, p: Dom => Dom) extends Dom
  case class LamD(f: Dom => Dom) extends Dom
  case class AppD(p: Dom, q: Dom) extends Dom
  case class SigD(a: Dom, p: Dom => Dom) extends Dom
  case class PairD(p: Dom, q: Dom) extends Dom
  case class FstD(d: Dom) extends Dom
  case class SndD(d: Dom) extends Dom

  def fstD(d: Dom) : Dom =
    d match {
      case PairD(p, _) => p
      case _ => FstD(d)
    }

  def sndD(d: Dom): Dom =
    d match {
      case PairD(_, q) => q
      case _ => SndD(d)
    }

  def appD(fd: Dom, d: Dom): Dom =
    fd match {
      case LamD(f) => f(d)
      case _ => AppD(fd, d)
    }

  //
  //  Reification and Reflection
  //

  def up(t: Dom, v: Dom): Dom =
    t match {
      case PiD(a, p) => LamD(d => up(p(d), AppD(v, down(a, d))))
      case SigD(a, p) => {
        val x = up(a, fstD(v))
        PairD(x, up(p(x), SndD(v)))
      }
      case UnitD => TtD
      case _ => v
    }

  def down(t: Dom, v: Dom): Dom =
    t match {
      case TypeD => downT(v)
      case UnitD => TtD
      case PiD(a, p) => {
        LamD(d => {
          val x = up(a, d)
          down(p(x), appD(v, x))
        })
      }
      case SigD(a, p) => {
        val x = fstD(v)
        PairD(down(a, x), down(p(x), sndD(v)))
      }
      case _ => v
    }

  def downT(t: Dom): Dom =
    t match {
      case PiD(a, p) => PiD(downT(a), d => downT(p(up(a, d))))
      case SigD(a, p) => SigD(downT(a), d => downT(p(up(a, d))))
      case _ => t
    }

  //
  // Readback
  //

  def rb(k: Int, d: Dom): Term =
    d match {
      case TtD => Tt
      case TypeD => Type
      case UnitD => Unt
      case VarD(i) => Var(k-i-1)
      case PiD(a, p) => Pi(rb(k, a), rb(k+1, p(VarD(k))))
      case LamD(f) => Lam(rb(k+1, f(VarD(k))))
      case AppD(u, v) => App(rb(k, u), rb(k, v))
      case SigD(a, p) => Sig(rb(k, a), rb(k+1, p(VarD(k))))
      case PairD(u, v) => Pair(rb(k, u), rb(k, v))
      case FstD(u) => Fst(rb(k, u))
      case SndD(u) => Snd(rb(k, u))
    }

  //
  // Evaluation
  //

  def eval(t: Term, rho: Rho): Dom =
    t match {
      case Tt => TtD
      case Type => TypeD
      case Unt => UnitD
      case Var(i) => rho(i)
      case Pi(a, p) => PiD(eval(a, rho), d => eval(p, UpVar(rho, PUnit(), d)))
      case Lam(u) => LamD(d => eval(u, UpVar(rho, PUnit(), d)))
      case App(u, v) => appD(eval(u, rho), eval(v, rho))
      case Sig(a, p) => SigD(eval(a, rho), d => eval(p, UpVar(rho, PUnit(), d)))
      case Pair(u, v) => PairD(eval(u, rho), eval(v, rho))
      case Fst(u) => fstD(eval(u, rho))
      case Snd(u) => sndD(eval(u, rho))
    }
  
  //
  //  Typechecking Environment
  //

  sealed trait Rho
  case object RNil extends Rho
  case class UpVar(rho: Rho, id: PattT, d: Dom) extends Rho
  case class UpDef(rho: Rho, id: Ident, ty: Term, tm: Term) extends Rho

  implicit class RhoOps(rho: Rho) {

    def apply(i: Int): Dom =
      rho match {
        case RNil => ???
        case UpVar(r, _, d) =>
          if (i <= 0) d else r(i-1)
        case UpDef(r, _, _, _) => r(i)
      }

    def apply(id: Ident): Dom =
      rho match {
        case RNil => ???
        case UpVar(r, PUnit(), _) => r(id)
        case UpVar(r, PVar(id0), d) =>
          if (id == id0) d else r(id)
        case UpDef(r, id0, ty, tm) =>
          if (id == id0) eval(tm, r) else r(id)
      }

    def length: Int =
      rho match {
        case RNil => 0
        case UpVar(r, _, _) => r.length + 1
        case UpDef(r, _, _, _) => r.length
      }

  }

  //
  //  Typechecking Monad
  //

  case class TCEnv(val gma: List[(Ident, Dom)], val rho: Rho)
  case class TCM[A](val run : TCEnv => Except[A])

  implicit val tcmMonad: Monad[TCM] =
    new Monad[TCM] {
      def pure[A](a: A) : TCM[A] = TCM(_ => Xor.Right(a))
      def flatMap[A, B](m: TCM[A])(f: A => TCM[B]) : TCM[B] =
        TCM(env => m.run(env).flatMap(a => f(a).run(env)))
    }

  import tcmMonad._

  def ask: TCM[TCEnv] =
    TCM(env => Xor.Right(env))

  def local[A](f: TCEnv => TCEnv)(m: TCM[A]): TCM[A] =
    TCM(env => m.run(f(env)))

  def tcError[A](msg: String) : TCM[A] =
    TCM(env => Xor.Left(msg))

  def tcAttempt[A](o: Option[A], msg: String) =
    TCM(env => attempt(o, msg))

  //
  //  Typechecking Helpers
  //

  def withVar(p: PattT, d: Dom) : TCEnv => TCEnv =
    env => {
      val r = UpVar(env.rho, p, VarD(env.rho.length))
      p match {
        case PUnit() => TCEnv(env.gma, r)
        case PVar(id) => TCEnv((id, d) :: env.gma, r)
      }
    }

  def withDef(id: Ident, ty: Term, tyD: Dom, tm: Term) : TCEnv => TCEnv =
    env => TCEnv((id, tyD) :: env.gma, UpDef(env.rho, id, ty, tm))

  def tcDepth: TCM[Int] =
    for {
      env <- ask
    } yield env.rho.length

  def tcEval(t: Term): TCM[Dom] =
    for {
      env <- ask
    } yield eval(t, env.rho)

  def tcEqNf(u: Dom, v: Dom): TCM[Unit] =
    for {
      i <- tcDepth
      u0 = rb(i, downT(u))
      v0 = rb(i, downT(v))
      r <- (
        if (u0 == v0) pure(())
        else tcError("Type conversion error.\nExpected: " + u0.toString + "\nInferred: " + v0.toString)
      )
    } yield r

  def tcLookup(id: Ident): TCM[(Term, Dom)] = {
    for {
      env <- ask
      tyD <- env.gma.find(_._1 == id) match {
        case None => tcError("Lookup failed for id: " + id)
        case Some(r) => pure(r._2)
      }
      tmD = env.rho(id)
    } yield (rb(env.rho.length, tmD), tyD)
  }

  def tcExtPi(d: Dom): TCM[(Dom, Dom => Dom)] =
    d match {
      case PiD(a, p) => pure(a, p)
      case _ => tcError("extPi")
    }

  def tcExtSig(d: Dom): TCM[(Dom, Dom => Dom)] =
    d match {
      case SigD(a, p) => pure(a, p)
      case _ => tcError("extSig")
    }

  //
  //  Typechecking Rules
  //

  def check(exp: ExpT, ty: Dom): TCM[Term] =
    (exp, ty) match {
      case (EType(), TypeD) => pure(Type)
      case (EUnit(), TypeD) => pure(Unt)
      case (ETt(), UnitD) => pure(Tt)
      case (ELam(id, u), PiD(a, p)) =>
        local(withVar(id, a))(
          for {
            i <- tcDepth
            u0 <- check(u, p(up(a, VarD(i-1))))
          } yield Lam(u0)
        )
      case (EPair(u, v), SigD(a, p)) =>
        for {
          u0 <- check(u, a)
          uD <- tcEval(u0)
          v0 <- check(v, p(uD))
        } yield Pair(u0, v0)
      case (EPi(Nil, p), TypeD) => check(p, TypeD)
      case (EPi(PTele(id, a) :: as, p), TypeD) =>
        for {
          a0 <- check(a, TypeD)
          aD <- tcEval(a0)
          r0 <- local(withVar(id, aD))(
            check(EPi(as, p), TypeD)
          )
        } yield Pi(a0, r0)
      case (ESig(Nil, p), TypeD) => check(p, TypeD)
      case (ESig(PTele(id, a) :: as, p), TypeD) =>
        for {
          a0 <- check(a, TypeD)
          aD <- tcEval(a0)
          r0 <- local(withVar(id, aD))(
            check(ESig(as, p), TypeD)
          )
        } yield Sig(a0, r0)
      case (EArrow(u, v), TypeD) => check(EPi(PTele(PUnit(), u) :: Nil, v), TypeD)
      case (e, t) =>
        for {
          pr <- checkI(e)
          (tm, typ) = pr
          _ <- tcEqNf(t, typ)
        } yield tm
      case _ => tcError("Unimplemented")

    }

  def checkI(exp: ExpT): TCM[(Term, Dom)] =
    exp match {
      case EVar(id) => tcLookup(id)
      case EApp(u, v) =>
        for {
          pr0 <- checkI(u)
          (u0, uTy) = pr0
          pr1 <- tcExtPi(uTy)
          (a, p) = pr1
          v0 <- check(v, a)
          vD <- tcEval(v0)
        } yield (App(u0, v0), p(vD))
      case EFst(u) =>
        for {
          pr0 <- checkI(u)
          (u0, uTy) = pr0
          pr1 <- tcExtSig(uTy)
          (a, _) = pr1
        } yield (Fst(u0), a)
      case ESnd(u) =>
        for {
          pr0 <- checkI(u)
          (u0, uTy) = pr0
          uD <- tcEval(u0)
          pr1 <- tcExtSig(uTy)
          (a, p) = pr1
        } yield (Snd(u0), p(fstD(uD)))
      case _ => tcError("Cannot infer type of:" + exp.toString)
    }

}
