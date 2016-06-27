/**
  * TypeChecker.scala - TypeChecker for OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopictt

import pprint._
import Tokenizer._
import PrettyPrinter._

import fastparse.core.Parsed.Success
import fastparse.core.Parsed.Failure

object TypeChecker {

  def error(msg: String) : Null = 
    throw new IllegalArgumentException(msg)

  //============================================================================================
  // TYPES OF NAMED CONSTANTS
  //

  def parseExpr(e: String): Expr = 
    Parser.expr.parse(e) match {
      case Success(texpr, _) => {
        // println("Parsed expr: " + texpr.toString)
        texpr
      }
      case f @ Failure(_, _, e) => {
        println("Failure: " + f.msg)
        println(e.traced.trace)
        error("Parsing failed.")
      }
    }

  def parseType(t: String) : Val =
    eval(parseExpr(t), RNil)

  // Basic constants
  val monadType = parseType("Type")
  val indexOfType = parseType("Mnd -> Type")
  val consOfType = parseType("(M : Mnd) -> (i : IndexOf M) -> Type")

  val etaType = parseType("(M : Mnd) -> (i : IndexOf M) -> ConsOf M i")
  val etaIntro = parseType("(M : Mnd) -> (P : (IndexOf M) -> Type) -> (i : IndexOf M) -> (x : P i) -> (p :> M i (eta M i)) -> P [p]")
  val etaElim = parseType("(M : Mnd) -> (P : (IndexOf M) -> Type) -> (d : (p :> M i (eta M i)) -> P [p]) -> P i")

  val muType = parseType("(M : Mnd) -> (i : IndexOf M) -> (c : ConsOf M i) -> (d : (p :> M i c) -> ConsOf M [p]) -> ConsOf M i")
  val muIntro = parseType(
    "(M : Mnd) -> (P : (IndexOf M) -> Type) -> " + 
      "(i : IndexOf M) -> (c : ConsOf M i) -> " +
      "(d : (p :> M i c) -> ConsOf M [p]) -> " +
      "(e : (p :> M i c) -> (q :> M [p] (d @ p)) -> P [q]) -> " + 
      "(p :> M i (mu M i c d)) -> P [p]"
  )
  val muElim = parseType(
    "(M : Mnd) -> (P : (IndexOf M) -> Type) -> " + 
      "(i : IndexOf M) -> (c : ConsOf M i) -> " +
      "(d : (p :> M i c) -> ConsOf M [p]) -> " + 
      "(f : (p :> M i (mu M i c d)) -> P [p]) -> " +
      "(p :> M i c) -> (q :> M [p] (d @ p)) -> P [q]"
  )

  // The slice construction
  val sliceType = parseType("Mnd -> Mnd")
  val sliceConsType = parseType("(M : Mnd) -> (i : IndexOf M) -> (c : ConsOf M i) -> Type")
  val slcIndexExpr = parseExpr("\\M. (i : IndexOf M) * (ConsOf M i)")
  val slcConsExpr = parseExpr("\\M. \\p. SlCn M (fst p) (snd p)")
  val slcEtaExpr = parseExpr("\\M. \\p. box M (fst p) (snd p) (<q> eta M [q]) (<q> dot M [q])")

  val dotType = parseType("(M : Mnd) -> (i : IndexOf M) -> SlCn M i (eta M i)")
  val boxType = parseType(
    "(M : Mnd) -> (i : IndexOf M) -> (c : ConsOf M i) -> " +
      "(d : ( p :> M i c ) -> ConsOf M [p]) -> " +
      "(e : ( p :> M i c ) -> SlCn M [p] (d @ p) ) -> " +
      "SlCn M i (mu M i c d)"
  )

  // Id Monad Implementation
  // val idType = parseType("Mnd")
  // val idIndexExpr = parseExpr("Unit")
  // val idConsExpr = parseExpr("\\_. Unit")

  object EtaVal {
    def unapply(n: Neut): Option[(Val, Val)] = 
      n match {
        case App(App(Eta, m), i) => Some(m, i)
        case _ => None
      }
  }

  object MuVal {
    def unapply(n: Neut): Option[(Val, Val, Val, Val)] = 
      n match {
        case App(App(App(App(Mu, m), i), c), d) => Some(m, i, c, d)
        case _ => None
      }
  }

  object MuExpr {
    def apply(m: Expr, i: Expr, c: Expr, d: Expr): Expr = 
      EApp(EApp(EApp(EApp(EMu, m), i), c), d)
  }

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

      // Slice reduction ...
      case (Nt(IndexOf), Nt(App(Sl, m))) => app(eval(slcIndexExpr, RNil), m)
      case (Nt(ConsOf), Nt(App(Sl, m))) => app(eval(slcConsExpr, RNil), m)
      case (Nt(Eta), Nt(App(Sl, m))) => app(eval(slcEtaExpr, RNil), m)

      case (Nt(k), m) => Nt(App(k, m))
      case _ => 
        error("app " ++ v.toString ++ " " ++ w.toString)
    }

  def vfst(v: Val) : Val = 
    v match {
      case MuP(_, u, _) => u
      case Pair(u, _) => u
      case Nt(k) => Nt(Fst(k))
      case _ => 
        error("vfst " ++ v.toString)
    }

  def vsnd(v: Val) : Val =
    v match {
      case MuP(_, _, u) => u
      case Pair(_, u) => u
      case Nt(k) => Nt(Snd(k))
      case _ => 
        error("vsnd " ++ v.toString)
    }

  def sel(v: Val, w: Val) : Val = {
    // println("sel: " + v.toString + " @ " + w.toString)

    (v, w) match {
      case (Kap(f), v) => f * v
      case (Nt(k), m) => Nt(Sel(k, m))
      case _ => error("sel: " + v.toString + w.toString)
    }
  }

  def typeOf(v: Val) : Val = {
    v match {
      case EtaP(m, i) => i
      case MuP(m, p, q) => typeOf(q)
      case _ => TypeOf(v)
    }
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

  def eval(e0: Expr, rho: Rho) : Val = 
    e0 match {

      case EType => Type
      case EUnit => Unt
      case ETt => Tt
      case EId => Id

      case EMnd => Nt(Mnd)
      case EIndexOf => Nt(IndexOf)
      case EConsOf => Nt(ConsOf)

      case EEta => Nt(Eta)
      case EEtaI => Nt(EtaI)
      case EEtaE => Nt(EtaE)

      case EMu => Nt(Mu)
      case EMuI => Nt(MuI)
      case EMuE => Nt(MuE)

      case ESl => Nt(Sl)
      case ESlCn => Nt(SlCn)
      case EDot => Nt(Dot)
      case EBox => Nt(Box)

      case EDec(d, e) => eval(e, UpDec(rho, d))
      case EVar(x) => getRho(rho, x)
      case EPi(p, a, b) => Pi(eval(a, rho), Cl(p, b, rho))
      case ELam(p, e) => Lam(Cl(p, e, rho))
      case EApp(e1, e2) => app(eval(e1, rho), eval(e2, rho))
      case ESig(p, a, b) => Sig(eval(a, rho), Cl(p, b, rho))
      case EPair(e1, e2) => Pair(eval(e1, rho), eval(e2, rho))
      case EFst(e) => vfst(eval(e, rho))
      case ESnd(e) => vsnd(eval(e, rho))

      case EPhi(p, m, i, c, e) => Phi(eval(m, rho), eval(i, rho), eval(c, rho), Cl(p, e, rho))
      case EKap(p, e) => Kap(Cl(p, e, rho))

      case ESel(e, f) => sel(eval(e, rho), eval(f, rho))
      case ETypeOf(p) => typeOf(eval(p, rho))

      // case _ => error("eval: should have been desugared\n e = " ++ e0.toString)
    }

  //============================================================================================
  // READBACK FUNCTIONS
  //

  def rbV(i: Int, v0: Val) : Expr = {

    def pat(i: Int) : Patt = 
      PVar("G#" ++ i.toString)

    def gen(i: Int) : Val = 
      Nt(Gen(i, "G#"))

    v0 match {

      case Type => EType
      case Unt => EUnit
      case Tt => ETt
      case Id => EId

      case Pi(t, g) => EPi(pat(i), rbV(i, t), rbV(i+1, g * gen(i)))
      case Lam(f) => ELam(pat(i), rbV(i + 1, f * gen(i)))
      case Sig(t, g) => ESig(pat(i), rbV(i, t), rbV(i+1, g * gen(i)))
      case Pair(u, v) => EPair(rbV(i, u), rbV(i, v))

      case Kap(f) => EKap(pat(i), rbV(i + 1, f * gen(i)))

      case Phi(m, j, c, g) => {
        // println("reading back a phi ...")
        EPhi(pat(i), rbV(i, m), rbV(i, j), rbV(i, c), rbV(i+1, g * gen(i)))
      }

      // Can't seem to merge this with the function above ...
      case TypeOf(EtaP(m, j)) => {
        // println("Reducing eta place")
        rbV(i, j)
      }
      case TypeOf(MuP(m, p, q)) => {
        println("Reducing mu place")
        rbV(i, TypeOf(q))
      }
      case TypeOf(p) => ETypeOf(rbV(i, p))

      case PlaceOf(_, _, _) => error("Readback PlaceOf")
      case EtaP(_, _) => error("Readback etaP")
      case MuP(_, _, _) => error("Readback muP")

      case Nt(k) => rbN(i, k)

    }
  }

  def rbN(i: Int, k0: Neut) : Expr = 
    k0 match {

      case Mnd => EMnd
      case IndexOf => EIndexOf
      case ConsOf => EConsOf

      case Eta => EEta
      case EtaI => EEtaI
      case EtaE => EEtaE

      case Mu => EMu
      case MuI => EMuI
      case MuE => EMuE

      case Sl => ESl
      case SlCn => ESlCn
      case Dot => EDot
      case Box => EBox

      // Left unit reduction
      case MuVal(m, j, Nt(EtaVal(_, _)), d) => rbV(i, sel(d, EtaP(m, j)))

      // Right unit and associativity reduction
      case MuVal(m, j, c, d @ Kap(g)) => {

        val gen = Gen(i, "P#")
        val pvar = PVar("P#" + i.toString)

        (g * (Nt(gen))) match {
          case Nt(EtaVal(_, _)) => rbV(i, c)
          case Nt(MuVal(_, _, Nt(MuVal(_, _, cc, dd)), ee)) => {

            println("Associativity reduction condition ...")

            val mexpr = rbV(i, m)
            val jexpr = rbV(i, j)

            // I feel like the idea will be to abstract one of these
            // results somehow ... right. cc is an actual constructor.
            // dd is a decoration of it.  and ee is a decoration of the
            // resulting multiplication.

            // Right.  First step is to abstract over places of my guy and
            // return c.  So, as it stands, c is a val.
            // I can see what's going to happen: you're going to need eta
            // reduction for these guys.  But let's continue ...

            // Okay, but I think the idea is to make a clever substitution with
            // patterns so that you generate the appropriate projections.

            val inner = MuExpr(mexpr, jexpr, rbV(i, c), EKap(pvar, rbV(i, cc)))
            val middle = MuExpr(mexpr, jexpr, inner, EKap(pvar, rbV(i, dd)))
            val outer = MuExpr(mexpr, jexpr, middle, EUnit)

            outer

          }
          case dv => MuExpr(rbV(i, m), rbV(i, j), rbV(i, c), EKap(pvar, rbV(i+1, dv)))
        }
      }

      case Gen(j, x) => EVar(x.toString ++ j.toString)
      case App(k, m) => EApp(rbN(i, k), rbV(i, m))
      case Fst(k) => EFst(rbN(i, k))
      case Snd(k) => ESnd(rbN(i, k))

      case Sel(k, m) => ESel(rbN(i, k), rbV(i, m))

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

  import opetopic.mtl._
  import Xor._

  type ErrorMessage = String
  type G[A] = Xor[ErrorMessage, A]

  val M = Monad[G]
  import M._

  def fail[A](str: String) : G[A] = 
    Left(str)

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

  def eqNf(i: Int, m1: Nf, m2: Nf, msg: String = "") : G[Unit] = {
    val e1 = rbV(i, m1)
    val e2 = rbV(i, m2)

    if (e1 == e2)
      pure(())
    else
      fail("eqNf: " + e1.pprint + " =/= " + e2.pprint + msg)
  }

  //============================================================================================
  // TYPE EXTRACTION
  //

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

  //============================================================================================
  // TYPE CHECKING RULES
  //

  def genV(rho: Rho) : Val = 
    Nt(Gen(lRho(rho), "TC#"))

  def checkD(rho: Rho, gma: Gamma, decl: Decl) : G[Gamma] = 
    decl match {
      case d@(Def(p, a, e)) => {
        // println("Type: " + a.toString)
        // println("Expr: " + e.toString)
        for {
          _ <- check(rho, gma, a, Type)
          t = eval(a, rho)
          _ <- check(rho, gma, e, t)
          gma1 <- upG(gma, p, t, eval(e, rho))
          _ = println("Checked definition: " + p.pprint)
        } yield gma1
      }
      case d@(Drec(p, a, e)) => 
        for {
          _ <- check(rho, gma, a, Type)
          t = eval(a, rho)
          gen = genV(rho)
          gma1 <- upG(gma, p, t, gen)
          _ <- check(UpVar(rho, p, gen), gma1, e, t)
          v = eval(e, UpDec(rho, d))
          gma2 <- upG(gma, p, t, v)
        } yield gma2
      case Dnorm(_) => pure(gma)
    }

  def upP(rho: Rho, gma: Gamma, p: Patt, m: Val, i: Val, c: Val) : G[(Rho, Gamma, Val)] = 
    (p, c) match {
      case (Punit, Nt(App(App(Eta, _), _))) => pure(rho, gma, EtaP(m, i))
      case (PPair(p1, p2), Nt(App(App(App(App(Mu, mm), ii), cc), dd))) => {

        for {
          a <- upP(rho, gma, p1, mm, ii, cc) 
          (rho1, gma1, q1) = a
          b <- upP(rho1, gma1, p2, mm, typeOf(q1), sel(dd, q1))
          (rho2, gma2, q2) = b
        } yield (rho2, gma2, MuP(m, q1, q2))

      }
      case (PVar(x), c) => {
        val gen = genV(rho)
        pure(UpVar(rho, PVar(x), gen), (x, PlaceOf(m, i, c)) :: gma, gen)
      }
      case _ => fail("Could not match place pattern: " + p.pprint)
    }

  def check(rho: Rho, gma: Gamma, e0: Expr, t0: TVal) : G[Unit] = {

    (e0, t0) match {

      case (EType, Type) => pure(()) // Type in type
      case (ETt, Unt) => pure(())
      case (EUnit, Type) => pure(())

      case (EPi(p, a, b), Type) =>
        for {
          _ <- check(rho, gma, a, Type)
          gen = genV(rho)
          gma1 <- upG(gma, p, eval(a, rho), gen)
          _ <- check(UpVar(rho, p, gen), gma1, b, Type)
        } yield ()

      case (ELam(p, e), Pi(t, g)) => {
        val gen = genV(rho)

        for {
          gma1 <- upG(gma, p, t, gen)
          _ <- check(UpVar(rho, p, gen), gma1, e, g * gen)
        } yield ()
      }

      case (ESig(p, a, b), Type) => 
        check(rho, gma, EPi(p, a, b), Type)

      case (EPair(e1, e2), Sig(t, g)) => 
        for {
          _ <- check(rho, gma, e1, t)
          _ <- check(rho, gma, e2, g * eval(e1, rho))
        } yield ()

      // Print normalized terms for debugging
      case (EDec(d @ Dnorm(n), e), t) => {
        val nf = rbV(lRho(rho), eval(n, rho))
        println("Expression: " + n.pprint + " normalizes to " + nf.pprint)
        check(rho, gma, e, t)
      }

      case (EDec(d, e), t) => 
        for {
          gma1 <- checkD(rho, gma, d)
          _ <- check(UpDec(rho, d), gma1, e, t)
        } yield ()

      case (EKap(p, e), Phi(m, i, c, g)) => {
        for {
          rgv <- upP(rho, gma, p, m, i, c)
          (rho1, gma1, gen1) = rgv
          _ <- check(rho1, gma1, e, g * gen1)
        } yield ()
      }

      case (EPhi(p, m, i, c, e), Type) => 
        for {
          _ <- check(rho, gma, m, eval(EMnd, rho))
          _ <- check(rho, gma, i, eval(EApp(EIndexOf, m), rho))
          _ <- check(rho, gma, c, eval(EApp(EApp(EConsOf, m), i), rho))
          gen = genV(rho)
          gma1 <- upG(gma, p, PlaceOf(eval(m, rho), eval(i, rho), eval(c, rho)), gen)
          _ <- check(UpVar(rho, p, gen), gma1, e, Type)
        } yield ()

      case (e, t) => 
        for {
          t1 <- checkI(rho, gma, e)
          // Yeah, you should make this guy lazy so that you don't print an expression every time
          // you check the normal form ...
          _ <- eqNf(lRho(rho), t, t1, "\nwhile checking that " + e.pprint + " has type " + rbV(lRho(rho), t).pprint)
        } yield ()
    }
  }

  def checkP(rho: Rho, gma: Gamma, p: Expr, m: Val, i: Val, c: Val) : G[Unit] = 
    for {
      pt <- checkI(rho, gma, p) 
      _ <- pt match {
        case PlaceOf(mm, ii, cc) => {
          val l = lRho(rho)
          for {
            _ <- eqNf(l, m, mm)
            _ <- eqNf(l, i, ii)
            _ <- eqNf(l, c, cc)
          } yield ()
        }
        case _ => fail("checkP: " + p.pprint)
      }
    } yield ()

  def checkI(rho: Rho, gma: Gamma, e0: Expr) : G[TVal] = {
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

      case ETypeOf(p) => 
        for {
          t <- checkI(rho, gma, p)
          r <- t match {
            case PlaceOf(m, _, _) => pure(app(Nt(IndexOf), m))
            case _ => error("Not a place: " + p.pprint)
          }
        } yield r

      case ESel(d, p) => 
        for {
          t <- checkI(rho, gma, d)
          rt <- t match {
            case Phi(m, i, c, g) => 
              for {
                _ <- checkP(rho, gma, p, m, i, c)
              } yield (g * eval(p, rho))
            case _ => fail("Selection not a place quantifier")
          }
        } yield rt

      case ETt => pure(Unt)

      // case EId => pure(idType)
      case EMnd => pure(monadType)
      case EIndexOf => pure(indexOfType)
      case EConsOf => pure(consOfType)

      case EEta => pure(etaType)
      case EEtaI => pure(etaIntro)
      case EEtaE => pure(etaElim)

      case EMu => pure(muType)
      case EMuI => pure(muIntro)
      case EMuE => pure(muElim)

      case ESl => pure(sliceType)
      case ESlCn => pure(sliceConsType)
      case EDot => pure(dotType)
      case EBox => pure(boxType)

      case e => fail("checkI: " ++ e.toString)

    }
  }

}

