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
import OttPrettyPrinter._

object TypeChecker {

  //
  //  The Semantic Domain
  //

  sealed trait Dom
  case object TtD extends Dom
  case object TypeD extends Dom
  case object UnitD extends Dom
  case object CatD extends Dom
  case class VarD(i: Int) extends Dom
  case class PiD(a: Dom, p: Dom => Dom) extends Dom
  case class LamD(f: Dom => Dom) extends Dom
  case class AppD(p: Dom, q: Dom) extends Dom
  case class SigD(a: Dom, p: Dom => Dom) extends Dom
  case class PairD(p: Dom, q: Dom) extends Dom
  case class FstD(d: Dom) extends Dom
  case class SndD(d: Dom) extends Dom

  case class ObjD(d: Dom) extends Dom
  case class CellD(d: Dom, frm: SComplex[Dom]) extends Dom
  case class IsTgtUD(e : Dom) extends Dom
  case class IsSrcUD(e : Dom, addr : SAddr) extends Dom
  case class ReflD(e : Dom) extends Dom
  case class DropD(e : Dom) extends Dom
  case class CompD(pd : STree[Dom]) extends Dom
  case class FillD(pd : STree[Dom]) extends Dom
  case class LiftTgtD(e : Dom, ev : Dom, c : Dom, t : Dom) extends Dom
  case class LiftSrcD(e : Dom, ev : Dom, c : Dom, t : Dom) extends Dom
  case class FillTgtD(e : Dom, ev : Dom, c : Dom, t : Dom) extends Dom
  case class FillSrcD(e : Dom, ev : Dom, c : Dom, t : Dom) extends Dom
  case class DropIsTgtD(e : Dom) extends Dom
  case class FillIsTgtD(pd : STree[Dom]) extends Dom
  case class ShellIsTgtD(e : Dom, ev : Dom, pd : STree[Dom], tgt : Dom) extends Dom
  case class FillTgtIsTgtD(e : Dom, ev : Dom, c : Dom, t : Dom) extends Dom
  case class FillSrcIsTgtD(e : Dom, ev : Dom, c : Dom, t : Dom) extends Dom
  case class FillTgtIsSrcD(e : Dom, ev : Dom, c : Dom, t : Dom) extends Dom
  case class FillSrcIsSrcD(e : Dom, ev : Dom, c : Dom, t : Dom) extends Dom

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
      case CatD => Cat
      case VarD(i) => Var(k-i-1)
      case PiD(a, p) => Pi(rb(k, a), rb(k+1, p(VarD(k))))
      case LamD(f) => Lam(rb(k+1, f(VarD(k))))
      case AppD(u, v) => App(rb(k, u), rb(k, v))
      case SigD(a, p) => Sig(rb(k, a), rb(k+1, p(VarD(k))))
      case PairD(u, v) => Pair(rb(k, u), rb(k, v))
      case FstD(u) => Fst(rb(k, u))
      case SndD(u) => Snd(rb(k, u))
      case ObjD(cat) => Obj(rb(k, cat))
      case CellD(cat, frm) => Cell(rb(k, cat), frm.map(rb(k, _)))

      case IsTgtUD(e) => IsTgtU(rb(k, e))
      case IsSrcUD(e, addr) => IsSrcU(rb(k, e), addr) 
      case ReflD(e) => Refl(rb(k, e))
      case DropD(e) => Drop(rb(k, e))
      case CompD(pd) => Comp(pd.map(rb(k, _)))
      case FillD(pd) => Fill(pd.map(rb(k, _)))
      case LiftTgtD(e, ev, c, t) => LiftTgt(rb(k, e), rb(k, ev), rb(k, c), rb(k, t))
      case LiftSrcD(e, ev, c, t) => LiftSrc(rb(k, e), rb(k, ev), rb(k, c), rb(k, t))
      case FillTgtD(e, ev, c, t) => FillTgt(rb(k, e), rb(k, ev), rb(k, c), rb(k, t))
      case FillSrcD(e, ev, c, t) => FillSrc(rb(k, e), rb(k, ev), rb(k, c), rb(k, t))
      case DropIsTgtD(e) => DropIsTgt(rb(k, e))
      case FillIsTgtD(pd) => FillIsTgt(pd.map(rb(k, _)))
      case ShellIsTgtD(e, ev, pd, tgt) => ShellIsTgt(rb(k, e), rb(k, ev), pd.map(rb(k, _)), rb(k, tgt))
      case FillTgtIsTgtD(e, ev, c, t) => FillTgtIsTgt(rb(k, e), rb(k, ev), rb(k, c), rb(k, t))
      case FillSrcIsTgtD(e, ev, c, t) => FillSrcIsTgt(rb(k, e), rb(k, ev), rb(k, c), rb(k, t))
      case FillTgtIsSrcD(e, ev, c, t) => FillTgtIsSrc(rb(k, e), rb(k, ev), rb(k, c), rb(k, t))
      case FillSrcIsSrcD(e, ev, c, t) => FillSrcIsSrc(rb(k, e), rb(k, ev), rb(k, c), rb(k, t))

    }

  //
  // Evaluation
  //

  def eval(t: Term, rho: Rho): Dom =
    t match {
      case Tt => TtD
      case Type => TypeD
      case Unt => UnitD
      case Cat => CatD
      case Var(i) => rho(i)
      case Pi(a, p) => PiD(eval(a, rho), d => eval(p, UpVar(rho, PUnit(), d)))
      case Lam(u) => LamD(d => eval(u, UpVar(rho, PUnit(), d)))
      case App(u, v) => appD(eval(u, rho), eval(v, rho))
      case Sig(a, p) => SigD(eval(a, rho), d => eval(p, UpVar(rho, PUnit(), d)))
      case Pair(u, v) => PairD(eval(u, rho), eval(v, rho))
      case Fst(u) => fstD(eval(u, rho))
      case Snd(u) => sndD(eval(u, rho))
      case Obj(cat) => ObjD(eval(cat, rho))
      case Cell(cat, frm) => CellD(eval(cat, rho), frm.map(eval(_, rho)))

      case IsTgtU(e) => IsTgtUD(eval(e, rho))
      case IsSrcU(e, addr) => IsSrcUD(eval(e, rho), addr)
      case Refl(e) => ReflD(eval(e, rho))
      case Drop(e) => DropD(eval(e, rho))
      case Comp(pd) => CompD(pd.map(eval(_, rho)))
      case Fill(pd) => FillD(pd.map(eval(_, rho)))
      case LiftTgt(e, ev, c, t) => LiftTgtD(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case LiftSrc(e, ev, c, t) => LiftSrcD(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case FillTgt(e, ev, c, t) => FillTgtD(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case FillSrc(e, ev, c, t) => FillSrcD(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case DropIsTgt(e) => DropIsTgtD(eval(e, rho))
      case FillIsTgt(pd) => FillIsTgtD(pd.map(eval(_, rho)))
      case ShellIsTgt(e, ev, pd, tgt) => ShellIsTgtD(eval(e, rho), eval(ev, rho), pd.map(eval(_, rho)), eval(tgt, rho))
      case FillTgtIsTgt(e, ev, c, t) => FillTgtIsTgtD(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case FillSrcIsTgt(e, ev, c, t) => FillTgtIsTgtD(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case FillTgtIsSrc(e, ev, c, t) => FillTgtIsTgtD(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case FillSrcIsSrc(e, ev, c, t) => FillTgtIsTgtD(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
        
    }
  
  //
  //  Terms to user expressions
  //

  def treeToExp[A](t: STree[A])(w: A => TValT): TreeT =
    t match {
      case SLeaf => TLf()
      case SNode(a, sh) => TNd(w(a), treeToExp(sh)((b: STree[A]) => VTree(treeToExp(b)(w))))
    }

  def nstToExp(nst: SNesting[ExpT]): NstT =
    nst match {
      case SDot(e) => TDot(e)
      case SBox(e, cn) => TBox(e, treeToExp(cn)((n: SNesting[ExpT]) => VNst(nstToExp(n))))
    }

  def cmplxToExp(cmplx: SComplex[ExpT]): List[NstT] =
    Suite.SuiteTraverse.toList(cmplx).map(nstToExp(_))

  def addrToExp(addr: SAddr): AddrT =
    TAddr(addr.map((d: SDir) => addrToExp(d.dir)))

  def termToExp(k: Int, t: Term): ExpT =
    t match {
      case Tt => ETt()
      case Type => EType()
      case Unt => EUnit()
      case Cat => ECat()
      case Var(i: Int) => EVar("x" + (i + k - 1).toString) // Yeah, what's it again?
      case Pi(u, v) => EPi(PTele(PVar("x" + k.toString), termToExp(k, u)) :: Nil, termToExp(k+1, v))
      case Lam(u) => ELam(PVar("x" + k.toString), termToExp(k+1, u))
      case App(u, v) => EApp(termToExp(k, u), termToExp(k, v))
      case Sig(u, v) => ESig(PTele(PVar("x" + k.toString), termToExp(k, u)) :: Nil, termToExp(k+1, v))
      case Pair(u, v) => EPair(termToExp(k, u), termToExp(k, v))
      case Fst(u) => EFst(termToExp(k, u))
      case Snd(u) => ESnd(termToExp(k, u))

      case Obj(u) => EObj(termToExp(k, u))
      case Cell(u, frm) => ECell(termToExp(k, u), cmplxToExp(frm.map(termToExp(k, _))))
      case IsTgtU(e) => EIsTgtU(termToExp(k, e))
      case IsSrcU(e, addr) => EIsSrcU(termToExp(k, e), addrToExp(addr))
      case Refl(e) => ERefl(termToExp(k, e))
      case Drop(e) => EDrop(termToExp(k, e))
      case Comp(pd) => EComp(treeToExp(pd)((t: Term) => VExp(termToExp(k, t))))
      case Fill(pd) => EFill(treeToExp(pd)((t: Term) => VExp(termToExp(k, t))))
      case LiftTgt(e, ev, c, t) => ELiftTgt(termToExp(k, e), termToExp(k, ev), termToExp(k, c), termToExp(k, t))
      case LiftSrc(e, ev, c, t) => ELiftSrc(termToExp(k, e), termToExp(k, ev), termToExp(k, c), termToExp(k, t))
      case FillTgt(e, ev, c, t) => EFillTgt(termToExp(k, e), termToExp(k, ev), termToExp(k, c), termToExp(k, t))
      case FillSrc(e, ev, c, t) => EFillSrc(termToExp(k, e), termToExp(k, ev), termToExp(k, c), termToExp(k, t))
      case DropIsTgt(e) => EDropIsTgt(termToExp(k, e))
      case FillIsTgt(pd) => EFillIsTgt(treeToExp(pd)((t: Term) => VExp(termToExp(k, t))))
      case ShellIsTgt(e, ev, pd, tgt) => EShellIsTgt(termToExp(k, e), termToExp(k, ev), treeToExp(pd)((t: Term) => VExp(termToExp(k, t))), termToExp(k, tgt))
      case FillTgtIsTgt(e, ev, c, t) => EFillTgtIsTgt(termToExp(k, e), termToExp(k, ev), termToExp(k, c), termToExp(k, t))
      case FillSrcIsTgt(e, ev, c, t) => EFillSrcIsTgt(termToExp(k, e), termToExp(k, ev), termToExp(k, c), termToExp(k, t))
      case FillTgtIsSrc(e, ev, c, t) => EFillTgtIsSrc(termToExp(k, e), termToExp(k, ev), termToExp(k, c), termToExp(k, t))
      case FillSrcIsSrc(e, ev, c, t) => EFillSrcIsSrc(termToExp(k, e), termToExp(k, ev), termToExp(k, c), termToExp(k, t))
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

  type Gamma = List[(Ident, Dom)]
  case class TCEnv(val gma: Gamma, val rho: Rho)
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
  //  Trees, Nestings and Complexes
  //

  def tcParseNesting(n: NstT) : TCM[SNesting[ExpT]] =
    n match {
      case TDot(e) => pure(SDot(e))
      case TBox(e, ct) =>
        for {
          cn <- tcParseTree[SNesting[ExpT]](ct)({
            case VNst(n) => tcParseNesting(n)
            case _ => tcError("Unexpected value in nesting")
          })
        } yield SBox(e, cn)
    }

  def tcParseTreeExp(t: TreeT) : TCM[STree[ExpT]] =
    tcParseTree(t)({
      case VExp(e) => pure(e)
      case _ => tcError("Unexpected value in tree expression")
    })

  def tcParseTree[A](t: TreeT)(vParse: TValT => TCM[A]) : TCM[STree[A]] =
    t match {
      case TLf() => pure(SLeaf)
      case TNd(v, sh) =>
        for {
          a <- vParse(v)
          sh <- tcParseTree[STree[A]](sh)({
            case VTree(b) => tcParseTree[A](b)(vParse)
            case _ => tcError("Unexpected value in tree")
          })
        } yield SNode(a, sh)
    }

  // Expects the complex to be in reverse order, i.e.,
  // higher dimensions towards the front of the list ...
  def tcParseComplex(c: List[NstT]): TCM[SComplex[ExpT]] =
    c match {
      case Nil => tcError("Empty complex")
      case o :: Nil => tcParseNesting(o).map(||(_))
      case hd :: tl =>
        for {
          ctl <- tcParseComplex(tl)
          chd <- tcParseNesting(hd)
        } yield ctl >> chd
    }

  def tcParseAddr(a: AddrT): TCM[SAddr] =
    a match {
      case TAddr(ds) => ds.traverse(tcParseAddr(_).map(SDir(_)))
    }

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
        else tcError("Type conversion error.\nExpected: " + pprint(termToExp(i, u0)) + "\nInferred: " + pprint(termToExp(i, v0)))
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

  def tcInferCell(e: ExpT): TCM[(Term, Dom, SComplex[Dom])] =
    for {
      pr <- checkI(e)
      (tm, ty) = pr
      res <- ty match {
        case CellD(cat, frm) => pure(cat, frm)
        case _ => tcError("tcInferCell: " + pprint(e))
      }
    } yield { val (cat, frm) = res ; (tm, cat, frm) }

  def tcInferObject(e: ExpT): TCM[(Term, Dom)] =
    for {
      pr <- checkI(e)
      (tm, ty) = pr
      cat <- ty match {
        case ObjD(cat) => pure(cat)
        case _ => tcError("tcInferCell: " + pprint(e))
      }
    } yield (tm, cat)

  def tcInferComplex(pd: STree[ExpT]) : TCM[(STree[Term], Dom, SComplex[Dom], STree[SNesting[Dom]])] = {

    for {
      // First, we need to find the category we are working in 
      re <- tcAttempt(pd.rootValue, "Pasting diagram has no root")
      rt <- tcInferCell(re)
      (_, rcat, _) = rt

      pdPrs <- pd.traverse(e => {
        for {
          info <- tcInferCell(e)
          (tm, cat, frm) = info
          tmD <- tcEval(tm)
        } yield (tm, frm >> SDot(tmD))
      })

      pdPr = STree.unzip(pdPrs)
      (pdTm, pdD) = pdPr

      k <- tcDepth

      res <- tcAttempt(
        graft(pdD)((d0: Dom, d1: Dom) =>
          if (rb(k, d0) == rb(k, d1)) Some(d0) else None
        ),
        "Complex graft failed"
      )
    } yield { val (w, p) = res ; (pdTm, rcat, w, p) }

  }

  def tcInferSrcUniv(e: ExpT): TCM[(Term, SAddr)] =
    for {
      pr <- checkI(e)
      (tm, ev) = pr
      a <- ev match {
        case IsSrcUD(_, a) => pure(a)
        case _ => tcError("Evidence is not a source universal")
      }
    } yield (tm, a)

  def tcCellType(cat: Dom, c: SComplex[Dom]): TCM[Dom] =
    c match {
      case ||(SDot(_)) => pure(ObjD(cat))
      case frm >> SDot(_) => pure(CellD(cat, frm))
      case _ => tcError("tcCellType: not a cell")
    }

  def tcCompType(pd: STree[ExpT]): TCM[(STree[Term], Dom)] = 
    for {
      quad <- tcInferComplex(pd)
      (pdTm, cat, web, pdD) = quad
      cc <- tcAttempt((web >> SBox(TtD, pdD)).sourceAt(Nil), "Failed to get target")
      ct <- tcCellType(cat, cc)
    } yield (pdTm, ct)


  def tcFillType(pd: STree[ExpT]): TCM[(STree[Term], Dom)] =
    for {
      quad <- tcInferComplex(pd)
      (pdTm, cat, web, pdD) = quad
      srcs <- tcAttempt(pdD.traverse({
        case SDot(d) => Some(d)
        case _ => None
      }), "Not a pasting diagram")
    } yield (pdTm , CellD(cat, web >> SBox(CompD(srcs), pdD)))

  //
  //  Typechecking Rules
  //

  case class LiftData(
    eT: Term, evT: Term, cT: Term, tT: Term,
    eD: Dom, evD: Dom, cD: Dom, tD: Dom,
    catD: Dom, cellD: SComplex[Dom], addr: SAddr
  )

  def checkTgtLift(e: ExpT, ev: ExpT, c: ExpT, t: ExpT): TCM[LiftData] =
    for {
      pr <- tcInferCell(e)
      (eT, cat, frm) = pr
      eD <- tcEval(eT)
      evT <- check(ev, IsTgtUD(eD))
      evD <- tcEval(evT)
      cell = frm >> SDot(eD)
      cCell <- tcAttempt(cell.target, "Target failed")
      cellD <- tcCellType(cat, cCell)
      cT <- check(c, cellD)
      cD <- tcEval(cT)
      tFrm = frm.withTopValue(cD)
      tT <- check(t, CellD(cat, tFrm))
      tD <- tcEval(tT)
    } yield LiftData(eT, evT, cT, tT, eD, evD, cD, tD, cat, cell, Nil)

  def checkSrcLift(e: ExpT, ev: ExpT, c: ExpT, t: ExpT): TCM[LiftData] =
    for {
      pr <- tcInferCell(e)
      (eT, cat, frm) = pr
      eD <- tcEval(eT)
      srcEv <- tcInferSrcUniv(ev) 
      (evT, addr) = srcEv
      evD <- tcEval(evT)
      naddr = SDir(addr) :: Nil
      cell = frm >> SDot(eD)
      cCell <- tcAttempt(frm.sourceAt(naddr), "Source calculation failed")
      cellD <- tcCellType(cat, cCell)
      cT <- check(c, cellD)
      cD <- tcEval(cT)
      tNst <- tcAttempt(frm.head.replaceAt(naddr, cD), "Replacement failed")
      tT <- check(t, CellD(cat, frm.withHead(tNst)))
      tD <- tcEval(tT)
    } yield LiftData(eT, evT, cT, tT, eD, evD, cD, tD, cat, cell, addr)

  // This is pretty inefficient in that we re-evaluate all the
  // terms each time.  It would be better to return the semantic
  // evaluation of the complex at the same time.
  def checkComplex(c: SComplex[ExpT], cat: Dom): TCM[SComplex[Term]] =
    c match {
      case ||(objs) => objs.traverse(e => check(e, ObjD(cat))).map(||(_))
      case tl >> hd =>
        for {
          tmTl <- checkComplex(tl, cat)
          dummyCmplx = tmTl >> hd.map(_ => Tt)
          tmHd <- hd.traverseWithAddr((e, addr) =>
            for {
              face <- tcAttempt(dummyCmplx.sourceAt(addr), "Failed to calculate source")
              frm <- tcAttempt(face.tail, "Internal error: frame dimension")
              frmD <- (frm : SComplex[Term]).traverse(tcEval(_))
              t <- check(e, CellD(cat, frmD))
            } yield t
          )
        } yield tmTl >> tmHd
    }

  def check(exp: ExpT, ty: Dom): TCM[Term] = {

    // println("Checking: " ++ exp.toString)
    // println("has type: " ++ rb(0, ty).toString)

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


      case (ECat(), TypeD) => pure(Cat)
      case (EObj(c), TypeD) => check(c, CatD).map(Obj(_))

      case (ECell(c, f), TypeD) =>
        for {
          cT <- check(c, CatD)
          cD <- tcEval(cT)
          frm <- tcParseComplex(f.reverse)
          frmT <- checkComplex(frm, cD)
          _ <- tcAttempt(frmT.asFrame, "Not a frame")
        } yield Cell(cT, frmT)

      case (EIsTgtU(e), TypeD) =>
        for {
          trpl <- tcInferCell(e)
        } yield IsTgtU(trpl._1)

      case (EIsSrcU(e, a), TypeD) =>
        for {
          trpl <- tcInferCell(e)
          (eT, cat, frm) = trpl
          addr <- tcParseAddr(a)
          st <- tcAttempt(frm.asFrame, "Malformed frame")
          (srcs, tgt) = st
          _ <- tcAttempt(srcs.seekTo(addr), "Invalid address")
        } yield IsSrcU(eT, addr)

      case (e, t) =>
        for {
          pr <- checkI(e)
          (tm, typ) = pr
          _ <- tcEqNf(t, typ)
        } yield tm
      case _ => tcError("Unimplemented")

    }

  }

  def checkI(exp: ExpT): TCM[(Term, Dom)] =
    exp match {

      //
      //  Basic Inferences
      //
      
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


      //
      // Cell Construction
      // 

      case ERefl(e) => {
        for {
          pr <- checkI(e)
          (tm, ty) = pr
          tmD <- tcEval(tm)
          res <- ty match {
            case ObjD(cat) => pure(CellD(cat, ||(SBox(tmD, STree.obj(SDot(tmD))))))
            case CellD(cat, frm) => 
              for {
                st <- tcAttempt(frm.asFrame, "Malformed frame")
              } yield CellD(cat, frm >> SBox(tmD, SNode(SDot(tmD), st._1.asShell)))
            case _ => tcError("Cannot apply reflexivity to non-cell: " + pprint(e))
          }
        } yield (Refl(tm), res)
      }

      case EDrop(e) => 
        for {
          pr <- checkI(e)
          (eT, eTy) = pr
          eD <- tcEval(eT)
          dTy <- eTy match {
            case ObjD(cat) => pure(CellD(cat, ||(SDot(eD)) >> SBox(ReflD(eD), SLeaf)))
            case CellD(cat, frm) => pure(CellD(cat, frm >> SBox(ReflD(eD), SLeaf)))
            case _ => tcError("Cannot apply drop to non-cell: " + pprint(e))
          }
        } yield (Drop(eT), dTy)

      case EComp(pd) =>
        for {
          pdE <- tcParseTreeExp(pd)
          pdPr <- tcCompType(pdE)
          (pdTm, pdTy) = pdPr
        } yield (Comp(pdTm), pdTy)

      case EFill(pd) =>
        for {
          pdE <- tcParseTreeExp(pd)
          pdPr <- tcFillType(pdE)
          (pdTm, pdTy) = pdPr
        } yield (Fill(pdTm), pdTy)

      case ELiftTgt(e, ev, c, t) => 
        checkTgtLift(e, ev, c, t).flatMap({
          case LiftData(eT, evT, cT, tT, eD, evD, cD, tD, catD, cellD, _) =>
            for {
              lext <- tcAttempt(cellD.targetExtension(cD, TtD, tD), "Target extension failed")
              lCell <- tcAttempt(lext.sourceAt(SDir(Nil) :: Nil), "Failed to extract lift face")
              lTy <- tcCellType(catD, lCell)
            } yield (LiftTgt(eT, evT, cT, tT), lTy)
        })

      case EFillTgt(e, ev, c, t) => 
        checkTgtLift(e, ev, c, t).flatMap({
          case LiftData(eT, evT, cT, tT, eD, evD, cD, tD, catD, cellD, _) =>
            for {
              lext <- tcAttempt(
                cellD.targetExtension(cD, LiftTgtD(eD, evD, cD, tD), tD),
                "Target extension failed"
              )
            } yield (FillTgt(eT, evT, cT, tT), CellD(catD, lext))
        })
        
      case ELiftSrc(e, ev, c, t) => 
        checkSrcLift(e, ev, c, t).flatMap({
          case LiftData(eT, evT, cT, tT, eD, evD, cD, tD, catD, cellD, addr) =>
            for {
              i <- tcDepth
              rext <- tcAttempt(cellD.sourceExtension(addr)(cD, TtD, tD), "Source extension failed")
              lCell <- tcAttempt(rext.sourceAt(SDir(SDir(addr) :: Nil) :: Nil), "Source calculation failed")
              lTy <- tcCellType(catD, lCell)
            } yield (LiftSrc(eT, evT, cT, tT), lTy)
        })

      case EFillSrc(e, ev, c, t) => 
        checkSrcLift(e, ev, c, t).flatMap({
          case LiftData(eT, evT, cT, tT, eD, evD, cD, tD, catD, cellD, addr) =>
            for {
              rext <- tcAttempt(
                cellD.sourceExtension(addr)(cD, LiftSrcD(eD, evD, cD, tD), tD),
                "Source extension failed"
              )
            } yield (FillSrc(eT, evT, cT, tT), CellD(catD, rext))
        })

      //
      //  Property Inferences
      //

      case EDropIsTgt(e) =>
        for {
          pr <- checkI(EDrop(e))
          (dTm, dTy) = pr
          dD <- tcEval(dTm)
          Drop(eTm) = dTm
        } yield (DropIsTgt(eTm), IsTgtUD(dD))

      case EFillIsTgt(pd) => 
        for {
          pr <- checkI(EFill(pd))
          (fTm, fTy) = pr
          fD <- tcEval(fTm)
          Fill(pdTm) = fTm
        } yield (FillIsTgt(pdTm), IsTgtUD(fD))

      case EShellIsTgt(e, ev, pd, t) =>
        for {
          trpl <- tcInferCell(e)
          (eT, catD, frmD) = trpl
          eD <- tcEval(eT)
          evT <- check(ev, IsTgtUD(eD))
          frmData <- tcAttempt(frmD.asFrame, "Malformed frame")
          (eSrcs, eTgt) = frmData
          pdE <- tcParseTreeExp(pd)
          pdPr <- tcAttempt(eSrcs.matchWith(pdE), "Source trees don't match")
          eitherTr <- pdPr.traverse[TCM, Either[Dom, Term]]((pr : (Dom, ExpT)) => {
            val (d, u) = pr
            u match {
              case ETt() => pure(Left(d))
              case u => check(u, IsTgtUD(d)).map(Right(_))
            }
          })
          pr <- (eitherTr.toList.filter(_.isLeft), t) match {
            case (Nil, ETt()) => pure((Tt, IsTgtUD(eTgt)))
            case (List(Left(u)), tev) => check(tev, IsTgtUD(eTgt)).map((_, IsTgtUD(u)))
            case _ => tcError("Malformed shell evidence")
          }
          (tT, rTy) = pr
          pdT = eitherTr.map({
            case Left(_) => Tt
            case Right(tm) => tm
          })
        } yield (ShellIsTgt(eT, evT, pdT, tT), rTy)

      case EFillTgtIsTgt(e, ev, c, t) => 
        checkTgtLift(e, ev, c, t).map({
          case LiftData(eT, evT, cT, tT, eD, evD, cD, tD, catD, cellD, _) =>
            (FillTgtIsTgt(eT, evT, cT, tT), IsTgtUD(FillTgtD(eD, evD, cD, tD)))
        })

      case EFillSrcIsTgt(e, ev, c, t) => 
        checkSrcLift(e, ev, c, t).map({
          case LiftData(eT, evT, cT, tT, eD, evD, cD, tD, catD, cellD, _) =>
            (FillSrcIsTgt(eT, evT, cT, tT), IsTgtUD(FillSrcD(eD, evD, cD, tD)))
        })

      case EFillTgtIsSrc(e, ev, c, t) =>
        checkTgtLift(e, ev, c, t).map({
          case LiftData(eT, evT, cT, tT, eD, evD, cD, tD, catD, cellD, _) =>
            (FillTgtIsSrc(eT, evT, cT, tT), IsSrcUD(FillTgtD(eD, evD, cD, tD), Nil))
        })

      case EFillSrcIsSrc(e, ev, c, t) =>
        checkSrcLift(e, ev, c, t).map({
          case LiftData(eT, evT, cT, tT, eD, evD, cD, tD, catD, cellD, addr) =>
            (FillSrcIsSrc(eT, evT, cT, tT), IsSrcUD(FillSrcD(eD, evD, cD, tD),
              List(SDir(List(SDir(addr)))))
            )
        })

      case _ => tcError("Cannot infer type of:" + pprint(exp))

    }

}
