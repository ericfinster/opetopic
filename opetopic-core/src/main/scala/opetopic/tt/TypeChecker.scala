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
      case EOb(e) => Ob(Cl(Punit, e, rho)) 
      case ECell(e, frm) => Cell(Cl(Punit, e, rho), frm) 
      case EHom(e, frm) => Hom(Cl(Punit, e, rho), frm) 
      case EComp(e, fp, nch) => Comp(eval(e, rho), fp.map(EvalNstMap(rho)), nch map (eval(_, rho)))
      case EFill(e, fp, nch) => Fill(eval(e, rho), fp.map(EvalNstMap(rho)), nch map (eval(_, rho)))
      case EIsLeftExt(e) => IsLeftExt(eval(e, rho))
      case EIsRightExt(e, a) => IsRightExt(eval(e, rho), a)
      case EFillIsLeft(e, fp, nch) => FillIsLeft(eval(e, rho), fp.map(EvalNstMap(rho)), nch map (eval(_, rho)))
      case ELiftLeft(e, ev) => Nt(LiftLeft(eval(e, rho), eval(ev, rho)))
      case EFillLeft(e, ev) => Nt(FillLeft(eval(e, rho), eval(ev, rho)))
      case EFillLeftIsLeft(e, ev) => Nt(FillLeftIsLeft(eval(e, rho), eval(ev, rho)))
      case EFillLeftIsRight(e, ev) => Nt(FillLeftIsRight(eval(e, rho), eval(ev, rho)))
      case ELiftRight(e, ev) => Nt(LiftRight(eval(e, rho), eval(ev, rho)))
      case EFillRight(e, ev) => Nt(FillRight(eval(e, rho), eval(ev, rho)))
      case EFillRightIsLeft(e, ev) => Nt(FillRightIsLeft(eval(e, rho), eval(ev, rho)))
      case EFillRightIsRight(e, ev) => Nt(FillRightIsRight(eval(e, rho), eval(ev, rho)))
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
      case Ob(Cl(_, e, rho)) => EOb(rbV(i, eval(e, rho)))  
      case Hom(Cl(_, e, rho), frm) => EHom(rbV(i, eval(e, rho)), frm.map(FrmNormMap(i, rho)))
      case Cell(Cl(_, e, rho), frm) => ECell(rbV(i, eval(e, rho)), frm.map(FrmNormMap(i, rho)))
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
      case LiftLeft(v, vv) => ELiftLeft(rbV(i, v), rbV(i, vv))
      case FillLeft(v, vv) => EFillLeft(rbV(i, v), rbV(i, vv))
      case FillLeftIsLeft(v, vv) => EFillLeftIsLeft(rbV(i, v), rbV(i, vv))
      case FillLeftIsRight(v, vv) => EFillLeftIsRight(rbV(i, v), rbV(i, vv))
      case LiftRight(v, vv) => ELiftRight(rbV(i, v), rbV(i, vv))
      case FillRight(v, vv) => EFillRight(rbV(i, v), rbV(i, vv))
      case FillRightIsLeft(v, vv) => EFillRightIsLeft(rbV(i, v), rbV(i, vv))
      case FillRightIsRight(v, vv) => EFillRightIsRight(rbV(i, v), rbV(i, vv))
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
  def checkFrame[N <: Nat](n: N)(rho: Rho, gma: Gamma, cmplx: ExprComplex[N], cat: Expr) : G[Unit] = {
    case (Z, rho, gma, Complex(_, hd), cat) => {
      hd match {
        case Box(tgt, Pt(Obj(src))) => 
          for {
            _ <- check(rho, gma, src, Ob(Cl(Punit, cat, rho)))
            _ <- check(rho, gma, tgt, Ob(Cl(Punit, cat, rho)))
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
  def checkCell[N <: Nat](n: N)(rho: Rho, gma: Gamma, cmplx: ExprComplex[N], cat: Expr) : G[Unit] = {
    case (Z, rho, gma, Complex(_, Obj(e)), cat) => check(rho, gma, e, Ob(Cl(Punit, cat, rho)))
    case (Z, rho, gma, Complex(_, _), cat) => fail("checkCell: too many objects!")
    case (S(p: P), rho, gma, Complex(tl, Dot(e, _)), cat) => {
      // println("Check that expression " ++ prettyPrint(e) ++ " lives is frame " ++ tl.toString)
      for {
        _ <- checkFrame(p)(rho, gma, tl, cat)
        _ <- check(rho, gma, e, Cell(Cl(Punit, cat, rho), tl))
      } yield ()
    }
    case (S(p: P), rho, gma, Complex(tl, _), cat) => fail("checkCell: too many top cells!")
  }

  @natElim
  def extractCellType[N <: Nat](n: N)(cmplx: ExprComplex[N], cat: Expr, rho: Rho) : G[Val] = {
    case (Z, Complex(_, Obj(e)), cat, rho) => pure(Ob(Cl(Punit, cat, rho)))
    case (Z, Complex(_, _), cat, rho) => fail("Not an object")
    case (S(p), Complex(tl, Dot(e, _)), cat, rho) => pure(Cell(Cl(Punit, cat, rho), tl)) 
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
          _ <- checkFrame(c.dim)(rho, gma, c, e)
        } yield ()
      case (EHom(e, c), Cat) =>
        for {
          _ <- check(rho, gma, e, Cat)
          _ <- checkFrame(c.dim)(rho, gma, c, e)
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

    def extCellG(tv: TVal) : G[(Expr, Rho, Sigma[ExprComplex])] = 
      tv match {
        case Cell(Cl(_, e, rho), frm) => pure(e, rho, Sigma(frm.dim)(frm)) // pure((v, Sigma(c.dim)(c)))
        case u => fail("extCellG " ++ u.toString)
      }

    def extRightExt(tv: TVal) : G[(Val, Addr)] = 
      tv match {
        case IsRightExt(v, a) => pure((v, a))
        case u => fail("extRightExt: " ++ u.toString)
      }

    // def extLeftExt(tv: TVal) : G[Val] = 
    //   tv match {
    //     case LeftExt(v) => pure(v)
    //     case u => fail("extLeftExt: " ++ u.toString)
    //   }

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
            (face: ExprComplex[Nat]) => checkCell(cmplx.dim)(rho, gma, face, e)
          )
          ty <- extractCellType(frm._1.dim)(frm._1, e, rho)
        } yield ty

      }
      case EFill(e, fp, nch) => {

        val cmplx : ExprComplex[Nat] = 
          fp >> Box(EComp(e, fp, nch), nch.asPd)

        for {
          _ <- check(rho, gma, e, Cat)
          cm <- fromShape(cmplx.comultiply)
          frm <- extractFrame(cm.head)
          _ <- frm._2.traverse(
            (face: ExprComplex[Nat]) => checkCell(cmplx.dim)(rho, gma, face, e)
          )
        } yield Cell(Cl(Punit, e, rho), cmplx) 

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
          frmCmplx = pr._3
          addr <- parseAddress(frmCmplx.dim)(a)
          frm <- extractFrame(frmCmplx.head)
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
            (face: ExprComplex[Nat]) => checkCell(cmplx.dim)(rho, gma, face, e)
          )
        } yield IsLeftExt(eval(EFill(e, fp, nch), rho))

      }
      case ELiftLeft(e, ev) => {

        val l = lRho(rho)
        val tgtVar = "CV#" + l.toString
        val liftVar = "CV#" + (l+1).toString

        for {
          t <- checkI(rho, gma, e)
          cellInfo <- extCellG(t)
          _ <- check(rho, gma, ev, IsLeftExt(eval(e, rho)))
          lext <- {

            val frm = cellInfo._3
            val cell : ExprComplex[S[frm.N]] = 
              frm.value >> Dot(e, S(frm.dim))

            fromShape(cell.leftExtend(EVar(tgtVar), EEmpty, EVar(liftVar)))

          }
          // The are the cells whose frames we want to abstract over ...
          tgtCell <- fromShape(lext.target)
          tgtTy <- extractCellType(tgtCell.dim)(tgtCell, cellInfo._1, cellInfo._2)
          liftCell <- fromShape(lext.sourceAt(Nil))
          liftTyExpr <- extractCellTypeExpr(liftCell.dim)(liftCell, cellInfo._1)
          // ... and this is where the lift will land 
          resCell <- fromShape(lext.sourceAt(Nil :: Nil))
          resTyExpr <- extractCellTypeExpr(resCell.dim)(resCell, cellInfo._1)
        } yield Pi(tgtTy, Cl(PVar(tgtVar), EPi(PVar(liftVar), liftTyExpr, resTyExpr), rho))

        // FIXME: I'm not sure about the environment here in the closure.  Should
        //        we use the one recorded by the Cell instance?  Somehow it seems like yes ...


      }
      case EFillLeft(e, ev) => {

        val l = lRho(rho)
        val tgtVar = "CV#" + l.toString
        val liftVar = "CV#" + (l+1).toString

        for {
          t <- checkI(rho, gma, e)
          cellInfo <- extCellG(t)
          _ <- check(rho, gma, ev, IsLeftExt(eval(e, rho)))
          lext <- {

            val frm = cellInfo._3
            val cell : ExprComplex[S[frm.N]] = 
              frm.value >> Dot(e, S(frm.dim))

            // The expression for the lift
            val liftExpr = EApp(EApp(ELiftLeft(e, ev), EVar(tgtVar)), EVar(liftVar))

            fromShape(cell.leftExtend(EVar(tgtVar), liftExpr, EVar(liftVar)))

          }
          // The are the cells whose frames we want to abstract over ...
          tgtCell <- fromShape(lext.target)
          tgtTy <- extractCellType(tgtCell.dim)(tgtCell, cellInfo._1, cellInfo._2)
          liftCell <- fromShape(lext.sourceAt(Nil))
          liftTyExpr <- extractCellTypeExpr(liftCell.dim)(liftCell, cellInfo._1)
        } yield Pi(tgtTy, Cl(PVar(tgtVar), EPi(PVar(liftVar), liftTyExpr, ECell(cellInfo._1, lext)), rho))

      }
      case ELiftRight(e, ev) => {

        val l = lRho(rho)
        val extVar = "CV#" + l.toString
        val liftVar = "CV#" + (l+1).toString

        for {
          t <- checkI(rho, gma, e)
          cellInfo <- extCellG(t)
          u <- checkI(rho, gma, ev)
          pr <- extRightExt(u)
          _ <- eqNf(l, pr._1, eval(e, rho))
          pr2 <- {

            val frm = cellInfo._3
            val cell : ExprComplex[S[frm.N]] = 
              frm.value >> Dot(e, S(frm.dim))

            for {
              addr <- parseAddress(frm.dim)(pr._2)
              res <- fromShape(cell.rightExtend(addr)(EVar(extVar), EEmpty, EVar(liftVar)))
            } yield (res, addr)
          }
          rext = pr2._1
          addr = pr2._2
          // // The are the cells whose frames we want to abstract over ...
          extCell <- fromShape(rext.tail.sourceAt(addr :: Nil))
          extTy <- extractCellType(extCell.dim)(extCell, cellInfo._1, cellInfo._2)
          liftCell <- fromShape(rext.sourceAt(Nil))
          liftTyExpr <- extractCellTypeExpr(liftCell.dim)(liftCell, cellInfo._1)
          // ... and this is where the lift will land 
          resCell <- fromShape(rext.sourceAt((addr :: Nil) :: Nil))
          resTyExpr <- extractCellTypeExpr(resCell.dim)(resCell, cellInfo._1)
        } yield Pi(extTy, Cl(PVar(extVar), EPi(PVar(liftVar), liftTyExpr, resTyExpr), rho))

        // FIXME: I'm not sure about the environment here in the closure.  Should
        //        we use the one recorded by the Cell instance?  Somehow it seems like yes ...


      }
      // case ELift(ce, fp, nch, ev) => {

      //   val cmplx : ExprComplex[Nat] = 
      //     fp >> Box(EEmpty, toPd(nch))

      //   val cdim = cmplx.dim
      //   val cvar = "CV#" ++ lRho(rho).toString

      //   for {
      //     _ <- check(rho, gma, ce, Cat)
      //     cv = eval(ce, rho)
      //     cm <- fromShape(cmplx.comultiply)
      //     frm <- extractFrame(cm.head)
      //     ckResTr <- frm._2.traverseWithAddress[G, \/[Address[Nat], Unit]]({
      //       case (face, addr) => 
      //         face.headValue match {
      //           case EEmpty => pure(-\/(addr))
      //           case _ => for {
      //             _ <- checkCell(cmplx.dim)(rho, gma, face, cv)
      //           } yield \/-(())
      //         }
      //     })
      //     res <- (
      //       ckResTr.nodes.filter(_.isLeft) match {
      //         case -\/(addr) :: Nil => 
      //           for {
      //             tgtCell <- fromShape(Complex.sourceAt(cdim)(cmplx, Nil))
      //             tgtTy <- extractCellType(cdim)(tgtCell, cv, rho)
      //             srcCell <- fromShape(Complex.sourceAt(cdim)(cmplx, addr :: Nil))
      //             srcTyExpr <- extractCellTypeExpr(cdim)(srcCell, ce)
      //           } yield Pi(tgtTy, Cl(PVar(cvar), srcTyExpr, rho))
      //         case _ => fail("Malformed punctured niche.")
      //       }
      //     )
      //   } yield res

      // }
      // case EBal(e, fp, nch) => {

      //   val cmplx : ExprComplex[Nat] = 
      //     fp >> Box(EEmpty, toPd(nch))

      //   for {
      //     _ <- check(rho, gma, e, Cat)
      //     cv = eval(e, rho)
      //     cm <- fromShape(cmplx.comultiply)
      //     frm <- extractFrame(cm.head)
      //     _ <- frm._2.traverse(
      //       (face: ExprComplex[Nat]) => {
      //         face.headValue match {
      //           case EEmpty => pure(())  // Skip the empty face ...
      //           case _ => checkCell(cmplx.dim)(rho, gma, face, cv)
      //         }
      //       }
      //     )
      //     empties = frm._2.nodes.filter(_.headValue == EEmpty)
      //     res <- (
      //       if (empties.length != 1)
      //         fail("A balanced niche must have one empty cell!") 
      //       else pure(Type)
      //     )
      //   } yield res
      // }
      // case ELeftBal(ce, c, e, f) => {

      //   val cdim = c.dim
      //   val cc = c >> Dot(e, S(cdim))
      //   val cvar = "CV#" ++ lRho(rho).toString

      //   for {
      //     _ <- check(rho, gma, ce, Cat)
      //     cv = eval(ce, rho)
      //     _ <- checkCell(S(cdim))(rho, gma, cc, cv)
      //     _ <- check(rho, gma, f, LeftExt(eval(e, rho)))
      //     lext <- fromShape(
      //       Complex.leftExtension[ConstExpr, Nat](cdim)(cc, EVar(cvar), EEmpty)
      //     )
      //     tgtCell <- fromShape(cc.target)
      //     tgtTy <- extractCellType(cdim)(tgtCell, cv, rho)
      //   } yield Pi(tgtTy, Cl(PVar(cvar), EBal(ce, lext._1, lext._2), rho))
      // }
      // case ERightBal(ce, c, e, a, f) => {

      //   val cdim = c.dim
      //   val cc = c >> Dot(e, S(cdim))
      //   val cvar = "CV#" ++ lRho(rho).toString

      //   for {
      //     _ <- check(rho, gma, ce, Cat)
      //     cv = eval(ce, rho)
      //     _ <- checkCell(S(cdim))(rho, gma, cc, cv)
      //     _ <- check(rho, gma, f, RightExt(eval(e, rho), a))
      //     addr <- parseAddress(cdim)(a)
      //     rext <- fromShape(
      //       Complex.rightExtension[ConstExpr, Nat](cdim)(cc, addr, EVar(cvar), EEmpty)
      //     )
      //     srcCell <- fromShape(Complex.sourceAt(cdim)(c, addr :: Nil))
      //     srcTy <- extractCellType(cdim)(srcCell, cv, rho)
      //   } yield Pi(srcTy, Cl(PVar(cvar), EBal(ce, rext._1, rext._2), rho))

      // }
      // case EFillerCompLeftExt(e, fp, nch) => {

      //   for {
      //     _ <- check(rho, gma, e, Cat)
      //     cv = eval(e, rho)
      //     prTr <- nch.traverse({
      //       case EPair(c, ev) => pure((c, ev))
      //       case _ => fail("Not a pair")
      //     })
      //     (cTr, eTr) = Tree.unzip(prTr)
      //     (cmplx : ExprComplex[Nat]) = fp >> Box(EComp(e, fp, cTr), toPd(cTr))
      //     cm <- fromShape(cmplx.comultiply)
      //     frm <- extractFrame(cm.head)
      //     _ <- frm._2.traverse(
      //       (face: ExprComplex[Nat]) => checkCell(cmplx.dim)(rho, gma, face, cv)
      //     )
      //     _ <- prTr.traverse({
      //       case (c, ev) => check(rho, gma, ev, LeftExt(eval(c, rho)))
      //     })
      //   } yield LeftExt(eval(EComp(e, fp, cTr), rho))

      // }
      // case ELiftFiller(ce, fp, nch, ev) => {

      //   val cmplx : ExprComplex[Nat] = 
      //     fp >> Box(EEmpty, toPd(nch))

      //   val cdim = cmplx.dim
      //   val cvar = "CV#" ++ lRho(rho).toString

      //   for {
      //     _ <- check(rho, gma, ce, Cat)
      //     cv = eval(ce, rho)
      //     cm <- fromShape(cmplx.comultiply)
      //     frm <- extractFrame(cm.head)
      //     ckResTr <- frm._2.traverseWithAddress[G, \/[Address[Nat], Unit]]({
      //       case (face, addr) => 
      //         face.headValue match {
      //           case EEmpty => pure(-\/(addr))
      //           case _ => for {
      //             _ <- checkCell(cmplx.dim)(rho, gma, face, cv)
      //           } yield \/-(())
      //         }
      //     })
      //     res <- (
      //       ckResTr.nodes.filter(_.isLeft) match {
      //         case -\/(addr) :: Nil => 
      //           for {
      //             tgtCell <- fromShape(Complex.sourceAt(cdim)(cmplx, Nil))
      //             tgtTy <- extractCellType(cdim)(tgtCell, cv, rho)
      //             srcTr = nch map {
      //               case EEmpty => Nesting.external(cdim)(EApp(ELift(ce, fp, nch, ev), EVar(cvar)))
      //               case e => Nesting.external(cdim)(e)
      //             }
      //           } yield Pi(tgtTy, Cl(PVar(cvar), ECell(ce, fp >> Box(EVar(cvar), srcTr)), rho))
      //         case _ => fail("Malformed punctured niche.")
      //       }
      //     )
      //   } yield res

      // }
      // case ELiftFillerLeftExt(ce, fp, nch, ev) => {

      //   val cmplx : ExprComplex[Nat] = 
      //     fp >> Box(EEmpty, toPd(nch))

      //   val cdim = cmplx.dim
      //   val cvar = "CV#" ++ lRho(rho).toString

      //   for {
      //     _ <- check(rho, gma, ce, Cat)
      //     cv = eval(ce, rho)
      //     cm <- fromShape(cmplx.comultiply)
      //     frm <- extractFrame(cm.head)
      //     ckResTr <- frm._2.traverseWithAddress[G, \/[Address[Nat], Unit]]({
      //       case (face, addr) => 
      //         face.headValue match {
      //           case EEmpty => pure(-\/(addr))
      //           case _ => for {
      //             _ <- checkCell(cmplx.dim)(rho, gma, face, cv)
      //           } yield \/-(())
      //         }
      //     })
      //     tgtCell <- fromShape(Complex.sourceAt(cdim)(cmplx, Nil))
      //     tgtTy <- extractCellType(cdim)(tgtCell, cv, rho)
      //   } yield Pi(tgtTy, Cl(PVar(cvar), ELeftExt(EApp(ELiftFiller(ce, fp, nch, ev), EVar(cvar))), rho))

      // }
      // case EFillerLeftIsRight(ce, fp, nch, ev) => {

      //   val cmplx : ExprComplex[Nat] = 
      //     fp >> Box(EEmpty, toPd(nch))

      //   val cdim = cmplx.dim

      //   val l = lRho(rho)
      //   val cvar = "CV#" ++ l.toString
      //   val fvar = "CV#" ++ (l + 1).toString
      //   val evvar = "CV#" ++ (l + 2).toString

      //   for {
      //     _ <- check(rho, gma, ce, Cat)
      //     cv = eval(ce, rho)
      //     cm <- fromShape(cmplx.comultiply)
      //     frm <- extractFrame(cm.head)
      //     ckResTr <- frm._2.traverseWithAddress[G, \/[Address[Nat], Unit]]({
      //       case (face, addr) => 
      //         face.headValue match {
      //           case EEmpty => pure(-\/(addr))
      //           case _ => for {
      //             _ <- checkCell(cmplx.dim)(rho, gma, face, cv)
      //           } yield \/-(())
      //         }
      //     })
      //     res <- (
      //       ckResTr.nodes.filter(_.isLeft) match {
      //         case -\/(addr) :: Nil => 
      //           for {
      //             tgtCell <- fromShape(Complex.sourceAt(cdim)(cmplx, Nil))
      //             tgtTy <- extractCellType(cdim)(tgtCell, cv, rho)
      //             srcTr = nch map {
      //               case EEmpty => Nesting.external(cdim)(EApp(ELift(ce, fp, nch, ev), EVar(cvar)))
      //               case e => Nesting.external(cdim)(e)
      //             }
      //             fillExprTy = ECell(ce, fp >> Box(EVar(cvar), srcTr))
      //             fillPiExpr = EPi(PVar(fvar), fillExprTy, EPi(PVar(evvar), ELeftExt(EVar(fvar)), ERightExt(EVar(fvar), rbAddr(cdim)(addr))))
      //           } yield Pi(tgtTy, Cl(PVar(cvar), fillPiExpr, rho))
      //         case _ => fail("Malformed punctured niche.")
      //       }
      //     )
      //   } yield res
      // }
      case e => fail("checkI: " ++ e.toString)
    }

  }

}
