/**
  * TypeChecker.scala - OpetopicTT Type Checker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import opetopic._
import syntax.tree._
import syntax.nesting._
import syntax.complex._
import syntax.suite._

import scalaz.{Tree => _, _}
import PrettyPrinter._

object OpetopicTypeChecker {

  def error(msg: String) : Nothing = 
    throw new IllegalArgumentException(msg)

  //============================================================================================
  // FUNCTION CLOSURES
  //

  implicit class ClosOps(c: Clos) {

    def *(v: Val) : Val = 
      c match {
        case Cl(p, e, rho) => eval(e, UpVar(rho, p, v))
      }

  }

  //============================================================================================
  // VALUE OPERATIONS
  //

  def app(v: Val, w: Val) : Val = 
    (v, w) match {
      case (Lam(f), v) => f * v
      case (Nt(k), m) => Nt(App(k, m))
      case _ => 
        error("app " ++ v.toString ++ " " ++ w.toString)
    }

  def vfst(v: Val) : Val = 
    v match {
      case Pair(u, _) => u
      case Nt(k) => Nt(Fst(k))
      case _ => 
        error("vfst " ++ v.toString)
    }

  def vsnd(v: Val) : Val =
    v match {
      case Pair(_, u) => u
      case Nt(k) => Nt(Snd(k))
      case _ => 
        error("vsnd " ++ v.toString)
    }

  //============================================================================================
  // ENVIRONMENT
  //

  def getRho(r: Rho, x: Name) : Val = 
    r match {
      case UpVar(rho, p, v) if inPat(x, p) => patProj(p, x, v)
      case UpVar(rho, p, v) if true => getRho(rho, x)
      case UpDec(rho, Def(p, _, e)) if inPat(x, p) => patProj(p, x, eval(e, rho))
      case UpDec(rho, Def(p, _, e)) if true => getRho(rho, x)
      case rho0@UpDec(rho, Drec(p, _, e)) if inPat(x, p) => patProj(p, x, eval(e, rho0))
      case rho0@UpDec(rho, Drec(p, _, e)) if true => getRho(rho, x)
      case RNil => error("getRho: " ++ x)
    }

  def inPat(x: Name, p: Patt) : Boolean = 
    p match {
      case PVar(y) => x == y
      case PPair(p1, p2) => inPat(x, p1) || inPat(x, p2)
      case _ => false
    }

  def patProj(p: Patt, x: Name, v: Val) : Val = 
    p match {
      case PVar(y) => if (x == y) v else error("patProj")
      case PPair(p1, p2) if inPat(x, p1) => patProj(p1, x, vfst(v))
      case PPair(p1, p2) if inPat(x, p2) => patProj(p2, x, vsnd(v))
      case _ => error("patProj")
    }

  def lRho(rho: Rho) : Int = 
    rho match {
      case RNil => 0
      case UpVar(r, _, _) => lRho(r) + 1
      case UpDec(r, _) => lRho(r)
    }

  case class EvalMap(rho: Rho) extends IndexedMap[ConstExpr, ConstVal] {
    def apply[N <: Nat](n: N)(e: Expr) = eval(e, rho)
  }

  case class EvalNstMap(rho: Rho) extends IndexedMap[NstExpr, NstVal] {
    def apply[N <: Nat](n: N)(ne: NstExpr[N]) = ne map (eval(_, rho))
  }

  def eval(e0: Expr, rho: Rho) : Val = 
    e0 match {
      case EType => Type
      case EEmpty => Empty
      case EUnit => Unt
      case ETt => Tt
      case EDec(d, e) => eval(e, UpDec(rho, d))
      case ELam(p, e) => Lam(Cl(p, e, rho))
      case EPi(p, a, b) => Pi(eval(a, rho), Cl(p, b, rho))
      case ESig(p, a, b) => Sig(eval(a, rho), Cl(p, b, rho))
      case EFst(e) => vfst(eval(e, rho))
      case ESnd(e) => vsnd(eval(e, rho))
      case EApp(e1, e2) => app(eval(e1, rho), eval(e2, rho))
      case EVar(x) => getRho(rho, x)
      case EPair(e1, e2) => Pair(eval(e1, rho), eval(e2, rho))
      case EProj(f, e) => Proj(f, eval(e, rho))
      case ERec(fs) => Rec(for { Field(f, e) <- fs } yield (f , e), rho )
      case ECat => Cat
      // I've turned off hom reduction for the time being ...
      // case EOb(EHom(e, c)) => eval(ECell(e, c), rho)
      // case ECell(EHom(e, c), d) => eval(ECell(e, c.concat(d)), rho) // Cells in a hom category reduce ...
      // case EHom(EHom(e, c), d) => eval(EHom(e, c.concat(d)), rho) // Composition of homs reduces ...
      case EOb(ce) => Ob(eval(ce, rho))
      case ECell(ce, frm) => Cell(eval(ce, rho), frm.map(EvalMap(rho)))
      case EHom(ce, frm) => Hom(eval(ce, rho), frm.map(EvalMap(rho)))
      case EComp(e, fp, nch) => Comp(eval(e, rho), fp.map(EvalNstMap(rho)), nch map (eval(_, rho)))
      case EFill(e, fp, nch) => Fill(eval(e, rho), fp.map(EvalNstMap(rho)), nch map (eval(_, rho)))
      case EIsLeftExt(e) => IsLeftExt(eval(e, rho))
      case EIsRightExt(e, a) => IsRightExt(eval(e, rho), a)
      case EFillIsLeft(e, fp, nch) => FillIsLeft(eval(e, rho), fp.map(EvalNstMap(rho)), nch map (eval(_, rho)))
      case ELiftLeft(ce, frm, e, ev) => Nt(LiftLeft(eval(ce, rho), frm.map(EvalMap(rho)), eval(e, rho), eval(ev, rho)))
      case EFillLeft(ce, frm, e, ev) => Nt(FillLeft(eval(ce, rho), frm.map(EvalMap(rho)), eval(e, rho), eval(ev, rho)))
      case EFillLeftIsLeft(ce, frm, e, ev) => Nt(FillLeftIsLeft(eval(ce, rho), frm.map(EvalMap(rho)), eval(e, rho), eval(ev, rho)))
      case EFillLeftIsRight(ce, frm, e, ev) => Nt(FillLeftIsRight(eval(ce, rho), frm.map(EvalMap(rho)), eval(e, rho), eval(ev, rho)))
      case ELiftRight(ce, frm, e, ev, a) => Nt(LiftRight(eval(ce, rho), frm.map(EvalMap(rho)), eval(e, rho), eval(ev, rho), a))
      case EFillRight(ce, frm, e, ev, a) => Nt(FillRight(eval(ce, rho), frm.map(EvalMap(rho)), eval(e, rho), eval(ev, rho), a))
      case EFillRightIsLeft(ce, frm, e, ev, a) => Nt(FillRightIsLeft(eval(ce, rho), frm.map(EvalMap(rho)), eval(e, rho), eval(ev, rho), a))
      case EFillRightIsRight(ce, frm, e, ev, a) => Nt(FillRightIsRight(eval(ce, rho), frm.map(EvalMap(rho)), eval(e, rho), eval(ev, rho), a))
    }

  //============================================================================================
  // READBACK FUNCTIONS
  //


  case class RbMap(i: Int) extends IndexedMap[ConstVal, ConstExpr] {
    def apply[N <: Nat](n: N)(v: Val) = rbV(i, v)
  }

  case class RbNstMap(i: Int) extends IndexedMap[NstVal, NstExpr] {
    def apply[N <: Nat](n: N)(nv: NstVal[N]) = nv map (rbV(i, _))
  }

  case class FrmNormMap(i: Int, rho: Rho) extends IndexedMap[ConstExpr, ConstExpr] {
    def apply[N <: Nat](n: N)(e: Expr) = rbV(i, eval(e, rho))
  }

  def rbV(i: Int, v0: Val) : Expr = {

    def pat(i: Int) : Patt = 
      PVar("G#" ++ i.toString)

    def gen(i: Int) : Val = 
      Nt(Gen(i, "G#"))

    v0 match {
      case Type => EType
      case Empty => EEmpty
      case Unt => EUnit
      case Tt => ETt
      case Lam(f) => ELam(pat(i), rbV(i + 1, f * gen(i)))
      case Pair(u, v) => EPair(rbV(i, u), rbV(i, v))
      case Pi(t, g) => EPi(pat(i), rbV(i, t), rbV(i+1, g * gen(i)))
      case Sig(t, g) => ESig(pat(i), rbV(i, t), rbV(i+1, g * gen(i)))
      case Proj(f, v) => EProj(f, rbV(i, v))
      case Nt(k) => rbN(i, k)
      case Cat => ECat
      // Turn off hom reduction ...
      // case Ob(Hom(v, c)) => rbV(i, Cell(v, c))
      // case Hom(Hom(v, c), d) => rbV(i, Hom(v, c.concat(d)))
      // case Cell(Hom(v, c), d) => rbV(i, Cell(v, c.concat(d)))
      // case Ob(v) => EOb(rbV(i, v))
      case Ob(cv) => EOb(rbV(i, cv))
      case Hom(cv, fv) => EHom(rbV(i, cv), fv.map(RbMap(i)))
      case Cell(cv, fv) => ECell(rbV(i, cv), fv.map(RbMap(i)))
      case Comp(v, fp, nch) => EComp(rbV(i, v), fp.map(RbNstMap(i)), nch map (rbV(i, _)))
      case Fill(v, fp, nch) => EFill(rbV(i, v), fp.map(RbNstMap(i)), nch map (rbV(i, _)))
      case IsLeftExt(v) => EIsLeftExt(rbV(i, v))
      case IsRightExt(v, a) => EIsRightExt(rbV(i, v), a)
      case FillIsLeft(v, fp, nch) => EFillIsLeft(rbV(i, v), fp.map(RbNstMap(i)), nch map (rbV(i, _)))
    }

  }

  def rbN(i: Int, k0: Neut) : Expr = 
    k0 match {
      case Gen(j, x) => EVar(x.toString ++ j.toString)
      case App(k, m) => EApp(rbN(i, k), rbV(i, m))
      case Fst(k) => EFst(rbN(i, k))
      case Snd(k) => ESnd(rbN(i, k))
      case LiftLeft(cv, fv, v, ev) => ELiftLeft(rbV(i, cv), fv.map(RbMap(i)), rbV(i, v), rbV(i, ev))
      case FillLeft(cv, fv, v, ev) => EFillLeft(rbV(i, cv), fv.map(RbMap(i)), rbV(i, v), rbV(i, ev))
      case FillLeftIsLeft(cv, fv, v, ev) => EFillLeftIsLeft(rbV(i, cv), fv.map(RbMap(i)), rbV(i, v), rbV(i, ev))
      case FillLeftIsRight(cv, fv, v, ev) => EFillLeftIsRight(rbV(i, cv), fv.map(RbMap(i)), rbV(i, v), rbV(i, ev))
      case LiftRight(cv, fv, v, ev, a) => ELiftRight(rbV(i, cv), fv.map(RbMap(i)), rbV(i, v), rbV(i, ev), a)
      case FillRight(cv, fv, v, ev, a) => EFillRight(rbV(i, cv), fv.map(RbMap(i)), rbV(i, v), rbV(i, ev), a)
      case FillRightIsLeft(cv, fv, v, ev, a) => EFillRightIsLeft(rbV(i, cv), fv.map(RbMap(i)), rbV(i, v), rbV(i, ev), a)
      case FillRightIsRight(cv, fv, v, ev, a) => EFillRightIsRight(rbV(i, cv), fv.map(RbMap(i)), rbV(i, v), rbV(i, ev), a)
    }

  def rbRho(i: Int, r: Rho) : List[Expr] = 
    r match {
      case RNil => Nil
      case UpVar(rho, _, v) => rbV(i, v) :: rbRho(i, rho)
      case UpDec(rho, _) => rbRho(i, rho)
    }

  //============================================================================================
  // AN ERROR MONAD
  //

  import scalaz.\/
  import scalaz.-\/
  import scalaz.\/-
  import scalaz.Monad
  import scalaz.std.list._

  type ErrorMessage = String
  type G[A] = \/[ErrorMessage, A]

  val M = Monad[G]
  import M._

  def fail[A](str: String) : G[A] = 
    -\/(str)

  def fromShape[A](m: ShapeM[A]) : G[A] =
    m match {
      case -\/(ShapeError(msg)) => -\/(msg)
      case \/-(a) => \/-(a)
    }

  def fromOption[A](o: Option[A], msg: String) : G[A] = 
    o match {
      case None => -\/(msg)
      case Some(a) => \/-(a)
    }

  def eqNf(i: Int, m1: Nf, m2: Nf) : G[Unit] = {
    val e1 = rbV(i, m1)
    val e2 = rbV(i, m2)

    if (e1 == e2)
      pure(())
    else
      fail("eqNf: " ++ e1.toString ++ " =/= " ++ e2.toString)
  }

  //============================================================================================
  // TYPE ENVIRONMENT
  //

  def lookupG[A, B](a0: A, prs: List[(A, B)]) : G[B] = 
    prs.find({ case (a, b) => a == a0 }) match {
      case None => fail("lookupG: " ++ a0.toString)
      case Some((a, b)) => pure(b)
    }

  def upG(gma: Gamma, p: Patt, t: TVal, v: Val) : G[Gamma] = 
    (p, t) match {
      case (Punit, _) => pure(gma)
      case (PVar(x), t) => pure((x, t) :: gma)
      case (PPair(p1, p2), Sig(t, g)) => 
        for {
          gma1 <- upG(gma, p1, t, vfst(v))
          gma2 <- upG(gma1, p2, g * vfst(v), vsnd(v))
        } yield gma2
      case _ => fail("upG: p = " ++ p.toString)
    }

  //============================================================================================
  // TYPE CHECKING RULES
  //

  def genV(rho: Rho) : Val = 
    Nt(Gen(lRho(rho), "TC#"))

  def checkD(rho: Rho, gma: Gamma, decl: Decl) : G[Gamma] = 
    decl match {
      case d@(Def(p, a, e)) => 
        for {
          _ <- checkT(rho, gma, a)
          t = eval(a, rho)
          _ <- check(rho, gma, e, t)
          gma1 <- upG(gma, p, t, eval(e, rho))
        } yield gma1
      case d@(Drec(p, a, e)) => 
        for {
          _ <- checkT(rho, gma, a)
          t = eval(a, rho)
          gen = genV(rho)
          gma1 <- upG(gma, p, t, gen)
          _ <- check(UpVar(rho, p, gen), gma1, e, t)
          v = eval(e, UpDec(rho, d))
          gma2 <- upG(gma, p, t, v)
        } yield gma2
    }

  def checkT(rho: Rho, gma: Gamma, e0: Expr) : G[Unit] = 
    e0 match {
      case EPi(p, a, b) => 
        for {
          _ <- checkT(rho, gma, a)
          gma1 <- upG(gma, p, eval(a, rho), genV(rho))
          _ <- checkT(UpVar(rho, p, genV(rho)), gma1, b)
        } yield ()
      case ESig(p, a, b) => checkT(rho, gma, EPi(p, a, b))
      case EType => pure(())
      case EUnit => pure(())
      case ECat => pure(())
      case a => {
        // println("Forcing manual Type verification for: " ++ prettyPrint(a))
        check(rho, gma, a, Type)
      }
    }

  def extractPd[A, N <: Nat](tr: Tree[Nesting[A, N], N]) : G[Tree[A, N]] = 
    tr traverse {
      case Obj(a) => pure(a)
      case Dot(a, _) => pure(a)
      case _ => fail("Not external")
    }

  def extractFrame[A, N <: Nat](nst: Nesting[A, N]) : G[(A, Tree[A, N])] =
    nst match {
      case Box(a, cn) => for { pd <- extractPd(cn) } yield (a, pd)
      case _ => fail("Not a frame")
    }

  @natElim
  def checkFrame[N <: Nat](n: N)(rho: Rho, gma: Gamma, cmplx: ExprComplex[N], cat: Val) : G[Unit] = {
    case (Z, rho, gma, Complex(_, hd), cat) => {
      hd match {
        case Box(tgt, Pt(Obj(src))) => 
          for {
            _ <- check(rho, gma, src, Ob(cat))
            _ <- check(rho, gma, tgt, Ob(cat))
          } yield ()
        case _ => fail("checkFrame: failed in dimension 0")
      }
    }
    case (S(p: P), rho, gma, cmplx, cat) => {
      // println("Checking frame: " ++ c.toString)
      for {
        cm <- fromShape(cmplx.comultiply)
        frm <- extractFrame(cm.head)
        _ <- checkCell(S(p))(rho, gma, frm._1, cat)
        _ <- frm._2.traverse(
          (face: Complex[ConstExpr, S[P]]) => checkCell(S(p))(rho, gma, face, cat)
        )
      } yield ()
    }
  }

  @natElim
  def checkCell[N <: Nat](n: N)(rho: Rho, gma: Gamma, cmplx: ExprComplex[N], cat: Val) : G[Unit] = {
    case (Z, rho, gma, Complex(_, Obj(e)), cat) => check(rho, gma, e, Ob(cat))
    case (Z, rho, gma, Complex(_, _), cat) => fail("checkCell: too many objects!")
    case (S(p: P), rho, gma, Complex(tl, Dot(e, _)), cat) => {
      // println("Check that expression " ++ prettyPrint(e) ++ " lives is frame " ++ tl.toString)
      for {
        _ <- checkFrame(p)(rho, gma, tl, cat)
        _ <- check(rho, gma, e, Cell(cat, tl.map(EvalMap(rho))))
      } yield ()
    }
    case (S(p: P), rho, gma, Complex(tl, _), cat) => fail("checkCell: too many top cells!")
  }

  @natElim
  def extractCellType[N <: Nat](n: N)(cmplx: ExprComplex[N], cat: Expr, rho: Rho) : G[Val] = {
    case (Z, Complex(_, Obj(e)), cat, rho) => pure(Ob(eval(cat, rho)))
    case (Z, Complex(_, _), cat, rho) => fail("Not an object")
    case (S(p), Complex(tl, Dot(e, _)), cat, rho) => pure(Cell(eval(cat, rho), tl.map(EvalMap(rho))))
    case (S(p), Complex(tl, _), cat, rho) => fail("Not a cell")
  }

  @natElim
  def extractCellTypeExpr[N <: Nat](n: N)(cmplx: ExprComplex[N], ce: Expr) : G[Expr] = {
    case (Z, Complex(_, Obj(e)), ce) => pure(EOb(ce))
    case (Z, Complex(_, _), ce) => fail("Not an object")
    case (S(p), Complex(tl, Dot(e, _)), ce) => pure(ECell(ce, tl))
    case (S(p), Complex(tl, _), ce) => fail("Not a cell")
  }

  @natElim
  def parseAddress[N <: Nat](n: N)(a: Addr) : G[Address[N]] = {
    case (Z, AUnit) => pure(())
    case (Z, _) => fail("parseAdress: only unit in dim 0")
    case (S(p), ANil) => pure(Nil)
    case (S(p), ACons(hd, tl)) => 
      for {
        hd0 <- parseAddress(p)(hd)
        tl0 <- parseAddress(S(p))(tl)
      } yield hd0 :: tl0
    case (S(p), _) => fail("parseAdress: unexpected address expression")
  }

  @natElim
  def rbAddr[N <: Nat](n: N)(a: Address[N]) : Addr = {
    case (Z, ()) => AUnit
    case (S(p), Nil) => ANil
    case (S(p), hd :: tl) => ACons(rbAddr(p)(hd), rbAddr(S(p))(tl))
  }

  def check(rho: Rho, gma: Gamma, e0: Expr, t0: TVal) : G[Unit] =
    (e0, t0) match {
      case (ELam(p, e), Pi(t, g)) => {
        val gen = genV(rho)

        for {
          gma1 <- upG(gma, p, t, gen)
          _ <- check(UpVar(rho, p, gen), gma1, e, g * gen)
        } yield ()
      }
      case (EPair(e1, e2), Sig(t, g)) => 
        for {
          _ <- check(rho, gma, e1, t)
          _ <- check(rho, gma, e2, g * eval(e1, rho))
        } yield ()
      case (EUnit, Type) => pure(())
      case (ETt, Unt) => pure(())
      case (EPi(p, a, b), Type) =>
        for {
          _ <- check(rho, gma, a, Type)
          gen = genV(rho)
          gma1 <- upG(gma, p, eval(a, rho), gen)
          _ <- check(UpVar(rho, p, gen), gma1, b, Type)
        } yield ()
      case (ESig(p, a, b), Type) => 
        check(rho, gma, EPi(p, a, b), Type)
      case (EDec(d, e), t) => 
        for {
          gma1 <- checkD(rho, gma, d)
          _ <- check(UpDec(rho, d), gma1, e, t)
        } yield ()
      case (ECat, Type) => pure(())
      case (EOb(e), Type) =>
        for {
          _ <- check(rho, gma, e, Cat)
        } yield ()
      case (ECell(e, c), Type) => 
        for {
          _ <- check(rho, gma, e, Cat)
          _ <- checkFrame(c.dim)(rho, gma, c, eval(e, rho))
        } yield ()
      case (EHom(e, c), Cat) =>
        for {
          _ <- check(rho, gma, e, Cat)
          _ <- checkFrame(c.dim)(rho, gma, c, eval(e, rho))
        } yield ()
      case (e, t) => {
        for {
          t1 <- checkI(rho, gma, e)
          _ <- eqNf(lRho(rho), t, t1)
        } yield ()
      }
    }

  // Try to infer a type for the given expression
  def checkI(rho: Rho, gma: Gamma, e0: Expr) : G[TVal] = {

    def extPiG(tv: TVal) : G[(TVal, Clos)] = 
      tv match {
        case Pi(t, g) => pure((t, g))
        case u => fail("extPiG " ++ u.toString)
      }

    def extSigG(tv: TVal) : G[(TVal, Clos)] = 
      tv match {
        case Sig(t, g) => pure((t, g))
        case u => fail("extSigG " ++ u.toString)
      }

    def extCellG(tv: TVal) : G[(Val, Sigma[ValComplex])] = 
      tv match {
        case Cell(cv, fv) => pure(cv, Sigma(fv.dim)(fv)) 
        case u => fail("extCellG " ++ u.toString)
      }

    e0 match {
      case EVar(x) => lookupG(x, gma)
      case EApp(e1, e2) =>
        for {
          t1 <- checkI(rho, gma, e1)
          pr <- extPiG(t1)
          (t, g) = pr
          _ <- check(rho, gma, e2, t)
        } yield (g * eval(e2, rho))
      case EFst(e) =>
        for {
          t1 <- checkI(rho, gma, e)
          pr <- extSigG(t1)
        } yield pr._1
      case ESnd(e) =>
        for {
          t <- checkI(rho, gma, e)
          pr <- extSigG(t)
          (_, g) = pr
        } yield g * vfst(eval(e, rho))
      case (cmp @ EComp(e, fp, nch)) => {

        val cmplx : ExprComplex[Nat] = 
          fp >> Box(cmp, nch.asPd)

        for {
          _ <- check(rho, gma, e, Cat)
          cm <- fromShape(cmplx.comultiply)
          frm <- extractFrame(cm.head)
          _ <- frm._2.traverse(
            (face: ExprComplex[Nat]) => checkCell(cmplx.dim)(rho, gma, face, eval(e, rho))
          )
          ty <- extractCellType(frm._1.dim)(frm._1, e, rho)
        } yield ty

      }
      case EFill(e, fp, nch) => {

        val cmplx : ExprComplex[Nat] = 
          fp >> Box(EComp(e, fp, nch), nch.asPd)

        for {
          _ <- check(rho, gma, e, Cat)
          cv = eval(e, rho)
          cm <- fromShape(cmplx.comultiply)
          frm <- extractFrame(cm.head)
          _ <- frm._2.traverse(
            (face: ExprComplex[Nat]) => checkCell(cmplx.dim)(rho, gma, face, cv)
          )
        } yield Cell(cv, cmplx.map(EvalMap(rho))) 

      }
      case EIsLeftExt(e) => {
        for {
          t <- checkI(rho, gma, e)
          pr <- extCellG(t)
        } yield Type
      }
      case EIsRightExt(e, a) => {
        for {
          t <- checkI(rho, gma, e)
          pr <- extCellG(t)
          fv = pr._2
          addr <- parseAddress(fv.dim)(a)
          frm <- extractFrame(fv.head)
          _ <- fromShape(frm._2.seekTo(addr))  // Seek to the address to make sure it's valid
        } yield Type
      }
      case EFillIsLeft(e, fp, nch) => {

        val cmplx : ExprComplex[Nat] = 
          fp >> Box(EComp(e, fp, nch), nch.asPd)

        for {
          _ <- check(rho, gma, e, Cat)
          cv = eval(e, rho)
          cm <- fromShape(cmplx.comultiply)
          frm <- extractFrame(cm.head)
          _ <- frm._2.traverse(
            (face: ExprComplex[Nat]) => checkCell(cmplx.dim)(rho, gma, face, cv)
          )
        } yield IsLeftExt(eval(EFill(e, fp, nch), rho))

      }
      case ELiftLeft(ce, frm, e, ev) => {

        val l = lRho(rho)

        val cVar = "CV#" + l.toString      // The competitor
        val tVar = "CV#" + (l+1).toString  // The target

        for {
          _ <- check(rho, gma, ce, Cat)
          cv = eval(ce, rho)
          d = S(frm.dim)
          cell = frm >> Dot(e, d)
          _ <- checkCell(d)(rho, gma, cell, cv)
          _ <- check(rho, gma, ev, IsLeftExt(eval(e, rho)))
          lext <- fromShape(cell.leftExtend(EVar(cVar), EEmpty, EVar(tVar)))
          cCell <- fromShape(lext.target)
          cTyExpr <- extractCellTypeExpr(cCell.dim)(cCell, ce)
          tCell <- fromShape(lext.sourceAt(Nil))
          tTyExpr <- extractCellTypeExpr(tCell.dim)(tCell, ce)
          lCell <- fromShape(lext.sourceAt(Nil :: Nil))
          lTyExpr <- extractCellTypeExpr(lCell.dim)(lCell, ce)
        } yield eval(EPi(PVar(cVar), cTyExpr, EPi(PVar(tVar), tTyExpr, lTyExpr)), rho)

      }
      case EFillLeft(ce, frm, e, ev) => {

        val l = lRho(rho)

        val cVar = "CV#" + l.toString      // The competitor
        val tVar = "CV#" + (l+1).toString  // The target

        val lExpr = EApp(EApp(ELiftLeft(ce, frm, e, ev), EVar(cVar)), EVar(tVar))

        for {
          _ <- check(rho, gma, ce, Cat)
          cv = eval(ce, rho)
          d = S(frm.dim)
          cell = frm >> Dot(e, d)
          _ <- checkCell(d)(rho, gma, cell, cv)
          _ <- check(rho, gma, ev, IsLeftExt(eval(e, rho)))
          lext <- fromShape(cell.leftExtend(EVar(cVar), lExpr, EVar(tVar)))
          cCell <- fromShape(lext.target)
          cTyExpr <- extractCellTypeExpr(cCell.dim)(cCell, ce)
          tCell <- fromShape(lext.sourceAt(Nil))
          tTyExpr <- extractCellTypeExpr(tCell.dim)(tCell, ce)
        } yield eval(EPi(PVar(cVar), cTyExpr, EPi(PVar(tVar), tTyExpr, ECell(ce, lext))), rho)

      }
      case EFillLeftIsLeft(ce, frm, e, ev) => {

        val l = lRho(rho)

        val cVar = "CV#" + l.toString      // The competitor
        val tVar = "CV#" + (l+1).toString  // The target

        val lExpr = EApp(EApp(ELiftLeft(ce, frm, e, ev), EVar(cVar)), EVar(tVar))

        for {
          _ <- check(rho, gma, ce, Cat)
          cv = eval(ce, rho)
          d = S(frm.dim)
          cell = frm >> Dot(e, d)
          _ <- checkCell(d)(rho, gma, cell, cv)
          _ <- check(rho, gma, ev, IsLeftExt(eval(e, rho)))
          lext <- fromShape(cell.leftExtend(EVar(cVar), lExpr, EVar(tVar)))
          cCell <- fromShape(lext.target)
          cTyExpr <- extractCellTypeExpr(cCell.dim)(cCell, ce)
          tCell <- fromShape(lext.sourceAt(Nil))
          tTyExpr <- extractCellTypeExpr(tCell.dim)(tCell, ce)
        } yield eval(
          EPi(PVar(cVar), cTyExpr, 
            EPi(PVar(tVar), tTyExpr, 
              EIsLeftExt(EApp(EApp(EFillLeft(ce, frm, e, ev), EVar(cVar)), EVar(tVar)))
            )), rho)

      }
      case EFillLeftIsRight(ce, frm, e, ev) => {

        val l = lRho(rho)

        val cVar = "CV#" + l.toString      // The competitor
        val tVar = "CV#" + (l+1).toString  // The target
        val lVar = "CV#" + (l+2).toString  // The lift
        val fVar = "CV#" + (l+3).toString  // The fill
        val evVar = "CV" + (l+4).toString  // The evidence

        for {
          _ <- check(rho, gma, ce, Cat)
          cv = eval(ce, rho)
          d = S(frm.dim)
          cell = frm >> Dot(e, d)
          _ <- checkCell(d)(rho, gma, cell, cv)
          _ <- check(rho, gma, ev, IsLeftExt(eval(e, rho)))
          lext <- fromShape(cell.leftExtend(EVar(cVar), EVar(lVar), EVar(tVar)))
          cCell <- fromShape(lext.target)
          cTyExpr <- extractCellTypeExpr(cCell.dim)(cCell, ce)
          tCell <- fromShape(lext.sourceAt(Nil))
          tTyExpr <- extractCellTypeExpr(tCell.dim)(tCell, ce)
          lCell <- fromShape(lext.sourceAt(Nil :: Nil))
          lTyExpr <- extractCellTypeExpr(lCell.dim)(lCell, ce)
        } yield eval(
          EPi(PVar(cVar), cTyExpr,
            EPi(PVar(tVar), tTyExpr, 
              EPi(PVar(lVar), lTyExpr, 
                EPi(PVar(fVar), ECell(ce, lext), 
                  EPi(PVar(evVar), EIsLeftExt(EVar(fVar)),
                    EIsRightExt(EVar(fVar), ACons(ANil, ANil))  // CHECK: Is this address correct?
                  ))))), rho)

      }
      case ELiftRight(ce, frm, e, ev, a) => {

        val l = lRho(rho)

        val cVar = "CV#" + l.toString      // The competitor
        val tVar = "CV#" + (l+1).toString  // The target

        for {
          _ <- check(rho, gma, ce, Cat)
          cv = eval(ce, rho)
          d = S(frm.dim)
          cell = frm >> Dot(e, d)
          _ <- checkCell(d)(rho, gma, cell, cv)
          _ <- check(rho, gma, ev, IsRightExt(eval(e, rho), a))
          addr <- parseAddress(frm.dim)(a)
          rext <- fromShape(cell.rightExtend(addr)(EVar(cVar), EEmpty, EVar(tVar)))
          cCell <- fromShape(rext.tail.sourceAt(frm.dim)(addr :: Nil))
          cTyExpr <- extractCellTypeExpr(cCell.dim)(cCell, ce)
          tCell <- fromShape(rext.sourceAt(Nil))
          tTyExpr <- extractCellTypeExpr(tCell.dim)(tCell, ce)
          lCell <- fromShape(rext.sourceAt((addr :: Nil) :: Nil))
          lTyExpr <- extractCellTypeExpr(lCell.dim)(lCell, ce)
        } yield eval(EPi(PVar(cVar), cTyExpr, EPi(PVar(tVar), tTyExpr, lTyExpr)), rho)

      }
      case e => fail("checkI: " ++ e.toString)
    }

  }

}
