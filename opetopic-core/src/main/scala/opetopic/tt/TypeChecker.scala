/**
  * TypeChecker.scala - TypeChecker for MiniTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import opetopic._
import opetopic.mtl._

object TypeChecker {

  def error(msg: String) : Null = 
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

      // Categories and Cells
      case ECat => Cat
      case EObj(c) => Obj(eval(c, rho))
      case ECell(c, frm) => Cell(eval(c, rho), frm.map(eval(_, rho)))

      // Propertes
      case EIsLeftExt(e) => IsLeftExt(eval(e, rho))
      case EIsRightExt(e, a) => IsRightExt(eval(e, rho), a)

      // Composition and identities
      case ERefl(e) => Refl(eval(e, rho))
      case EDrop(e) => Drop(eval(e, rho))
      case EComp(pd) => Comp(pd.map(eval(_, rho)))
      case EFill(pd) => Fill(pd.map(eval(_, rho)))

      // Liftings
      case ELiftLeft(e, ev, c, t) => LiftLeft(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case EFillLeft(e, ev, c, t) => FillLeft(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case ELiftRight(e, ev, c, t) => LiftRight(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case EFillRight(e, ev, c, t) => FillRight(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))

      case EDropIsLeft(c, e) => DropIsLeft(eval(c, rho), eval(e, rho))
      case EFillIsLeft(c, pd) => FillIsLeft(eval(c, rho), eval(pd, rho))
      case EShellIsLeft(e, ev, s, t) => ShellIsLeft(eval(e, rho), eval(ev, rho), eval(s, rho), eval(t, rho))

      // Derived properties
      case EFillLeftIsLeft(e, ev, c, t) => FillLeftIsLeft(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case EFillRightIsLeft(e, ev, c, t) => FillRightIsLeft(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case EFillLeftIsRight(e, ev, c, t) => FillLeftIsRight(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case EFillRightIsRight(e, ev, c, t) => FillRightIsRight(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))

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
      case Unt => EUnit
      case Type => EType
      case Tt => ETt
      case Empty => EEmpty
      case Lam(f) => ELam(pat(i), rbV(i + 1, f * gen(i)))
      case Pair(u, v) => EPair(rbV(i, u), rbV(i, v))
      case Pi(t, g) => EPi(pat(i), rbV(i, t), rbV(i+1, g * gen(i)))
      case Sig(t, g) => ESig(pat(i), rbV(i, t), rbV(i+1, g * gen(i)))

      case Cat => ECat
      case Obj(v) => EObj(rbV(i, v))
      case Cell(c, frm) => ECell(rbV(i, c), frm.map(rbV(i, _)))

      case IsLeftExt(v) => EIsLeftExt(rbV(i, v))
      case IsRightExt(v, a) => EIsRightExt(rbV(i, v), a)

      case Refl(v) => ERefl(rbV(i, v))
      case Drop(v) => EDrop(rbV(i, v))
      case Comp(pd) => EComp(pd.map(rbV(i, _)))
      case Fill(pd) => EFill(pd.map(rbV(i, _)))

      case LiftLeft(e, ev, c, t) => ELiftLeft(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))
      case FillLeft(e, ev, c, t) => EFillLeft(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))
      case LiftRight(e, ev, c, t) => ELiftRight(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))
      case FillRight(e, ev, c, t) => EFillRight(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))

      case DropIsLeft(c, v) => EDropIsLeft(rbV(i, c), rbV(i, v))
      case FillIsLeft(c, pd) => EFillIsLeft(rbV(i, c), rbV(i, pd))
      case ShellIsLeft(e, ev, s, t) => EShellIsLeft(rbV(i, e), rbV(i, ev), rbV(i, s), rbV(i, t))

      case FillLeftIsLeft(e, ev, c, t) => EFillLeftIsLeft(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))
      case FillRightIsLeft(e, ev, c, t) => EFillRightIsLeft(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))
      case FillLeftIsRight(e, ev, c, t) => EFillLeftIsRight(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))
      case FillRightIsRight(e, ev, c, t) => EFillRightIsRight(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))

      case Nt(k) => rbN(i, k)

    }
  }

  def rbN(i: Int, k0: Neut) : Expr = 
    k0 match {
      case Gen(j, x) => EVar(x.toString ++ j.toString)
      case App(k, m) => EApp(rbN(i, k), rbV(i, m))
      case Fst(k) => EFst(rbN(i, k))
      case Snd(k) => ESnd(rbN(i, k))
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

  type ErrorMessage = String
  type G[A] = Xor[ErrorMessage, A]

  val M = Monad[G]
  import M._

  def succeed[A](a: A): G[A] = 
    Xor.Right(a)

  def fail[A](str: String) : G[A] = 
    Xor.Left(str)

  def attempt[A](opt: Option[A], msg: String): G[A] = 
    opt match {
      case None => fail(msg)
      case Some(a) => Xor.Right(a)
    }

  def toOpt[A](m: G[A]): Option[A] = 
    m match {
      case Xor.Left(_) => None
      case Xor.Right(a) => Some(a)
    }

  //============================================================================================
  // TREE AND COMPLEX ROUTINES
  //

  type Frame[A] = (STree[A], A)

  def getFrame[A](c: SComplex[A]): G[Frame[A]] = 
    c.head match {
      case SBox(tgt, cn) => 
        for {
          srcTr <- attempt(cn.traverse(_.dotOption), "Not a frame")
        } yield (srcTr, tgt)
      case _ => fail("Not a frame")
    }

  def getSources[A](c: SComplex[A]): G[STree[A]] = 
    for { f <- getFrame(c) } yield f._1

  //============================================================================================
  // COMPLEX INFERENCE
  //

  def checkCell(rho: Rho, gma: Gamma, cell: SComplex[Expr], cat: Val): G[Unit] = 
    cell match {
      case ||(SDot(e)) => check(rho, gma, e, Obj(cat))
      case tl >> SDot(e) => 
        for {
          _ <- checkFrame(rho, gma, tl, cat)
          _ <- check(rho, gma, e, Cell(cat, (tl : SComplex[Expr]).map(eval(_, rho))))
        } yield ()
      case _ => fail("Not a cell")
    }

  def checkFrame(rho: Rho, gma: Gamma, frm: SComplex[Expr], cat: Val): G[Unit] = 
    frm match {
      case ||(SBox(tgt, SNode(SDot(src), SLeaf))) => 
        for {
          _ <- check(rho, gma, src, Obj(cat))
          _ <- check(rho, gma, tgt, Obj(cat))
        } yield ()
      case tl >> SBox(tgt, srcs) => 
        for {
          _ <- attempt(
            tl.head.toTree.matchWithAddr(srcs)({
              case (_, nst, addr) => 
                for {
                  _ <- nst.dotOption  // Make sure there are only dots
                  face <- frm.sourceAt(addr)
                  _ <- toOpt(checkCell(rho, gma, face, cat))
                } yield ()
            }),
            "Source checking failed"
          )
          tc <- attempt(frm.sourceAt(Nil), "Target calculation failed")
          _ <- checkCell(rho, gma, tc, cat)
        } yield ()
      case _ => fail("Malformed frame complex")
    }

  def inferCell(rho: Rho, gma: Gamma, e: Expr): G[(Val, SComplex[Val])] = 
    for {
      ty <- checkI(rho, gma, e)
      res <- ty match {
        case Cell(v, f) => pure(v, f)
        case _ => fail("inferCell: " + e.toString)
      }
    } yield res

  def inferComplex(rho: Rho, gma: Gamma, pd: STree[Expr]): G[(Val, SComplex[Val], STree[SNesting[Val]])] = {

    val l = lRho(rho)

    for {

      // Check the root so that we have a category to check against
      // You could be more efficient by then checking only the shell afterwards ...
      re <- attempt(pd.rootValue, "Cannot compose a leaf, use refl")
      rt <- inferCell(rho, gma, re)
      (rcat, _) = rt

      pdv <- pd.traverse[G, SComplex[Val]]((e: Expr) => {
        for {
          ty <- inferCell(rho, gma, e)
          (cat, frm) = ty
          v = eval(e, rho)
          _ <- eqNf(lRho(rho), rcat, cat)
        } yield frm >> SDot(v)
      })
      res <- attempt(
        graft(pdv)((v0: Val, v1: Val) => {
          for {
            _ <- toOpt(eqNf(l, v0, v1))
          } yield v0
        }),
        "Complex graft failed"
      )
    } yield { val (w, p) = res ; (rcat, w, p) }

  }

  def inferObject(rho: Rho, gma: Gamma, e: Expr) : G[Val] = 
    for {
      et <- checkI(rho, gma, e)
      res <- et match {
        case Obj(cv) => pure(cv)
        case _ => fail("Expression is not an object: " + e.toString)
      }
    } yield res

  def inferRightExt(rho: Rho, gma: Gamma, e: Expr) : G[SAddr] = 
    for {
      rev <- checkI(rho, gma, e)
      a <- rev match {
        case IsRightExt(_, addr) => pure(addr)
        case _ => fail("Evidence is not for a right extension")
      }
    } yield a

  def cellType(cat: Val, c: SComplex[Val]): G[Val] = 
    c match {
      case ||(SDot(_)) => pure(Obj(cat))
      case tl >> SDot(_) => pure(Cell(cat, tl))
      case _ => fail("cellType: not a cell")
    }

  // Right.  We don't check the object case here.  Probably should ...
  def compositeType(rho: Rho, gma: Gamma, pd: STree[Expr]): G[Val] = 
    for {
      trpl <- inferComplex(rho, gma, pd)
      (cat, web, pdv) = trpl
      cc <- attempt((web >> SBox(Empty, pdv)).sourceAt(Nil), "Failed to calculate target")
      ct <- cellType(cat, cc)
    } yield ct

  def fillType(rho: Rho, gma: Gamma, pd: STree[Expr]): G[Val] = 
    for {
      trpl <- inferComplex(rho, gma, pd)
      (cat, web, pdv) = trpl
    } yield Cell(cat, web >> SBox(Comp(pd.map(eval(_, rho))), pdv))

  //============================================================================================
  // TYPE ENVIRONMENT
  //

  type Gamma = List[(Name, TVal)]

  def lookupG[A, B](a0: A, prs: List[(A, B)]) : G[B] = 
    prs.find({ case (a, b) => a == a0 }) match {
      case None => fail("Variable not in scope: " + a0.toString)
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

  def eqNf(i: Int, m1: Nf, m2: Nf) : G[Unit] = {
    val e1 = rbV(i, m1)
    val e2 = rbV(i, m2)

    if (e1 == e2)
      pure(())
    else
      fail("eqNf: " ++ e1.toString ++ " =/= " ++ e2.toString)
  }

  //============================================================================================
  // TYPE CHECKING RULES
  //

  def genV(rho: Rho) : Val = 
    Nt(Gen(lRho(rho), "TC#"))

  def checkD(rho: Rho, gma: Gamma, decl: Decl) : G[Gamma] = 
    decl match {
      case d@(Def(p, a, e)) => {
        // println("Checking def with type: " + a.toString)
        // println("Checking def: " + e.toString)
        for {
          _ <- checkT(rho, gma, a)
          t = eval(a, rho)
          _ <- check(rho, gma, e, t)
          gma1 <- upG(gma, p, t, eval(e, rho))
          _ = println("Checked definition: " + p.toString)
        } yield gma1
      }
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
      case a => check(rho, gma, a, Type)
    }

  def check(rho: Rho, gma: Gamma, e0: Expr, t0: TVal) : G[Unit] = {

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
      case (ESig(p, a, b), Type) => 
        check(rho, gma, EPi(p, a, b), Type)
      case (EDec(d, e), t) => 
        for {
          gma1 <- checkD(rho, gma, d)
          _ <- check(UpDec(rho, d), gma1, e, t)
        } yield ()
      case (ECat, Type) => pure(())
      case (EObj(c), Type) =>
        for {
          _ <- check(rho, gma, c, Cat)
        } yield ()
      case (ECell(c, f), Type) => 
        for {
          _ <- check(rho, gma, c, Cat)
          cv = eval(c, rho)
          _ <- checkFrame(rho, gma, f, cv)
        } yield ()
      case (EIsLeftExt(e), Type) => 
        for {
          _ <- inferCell(rho, gma, e)
        } yield ()
      case (EIsRightExt(e, a), Type) => 
        for {
          pr <- inferCell(rho, gma, e)
          (c, f) = pr
          st <- getFrame(f)
          (srcs, tgt) = st
          _ <- attempt(srcs.seekTo(a), "Invalid address")
        } yield ()
      case (e, t) => 
        for {
          t1 <- checkI(rho, gma, e)
          _ <- eqNf(lRho(rho), t, t1)
        } yield ()
    }
  }

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

    e0 match {

      //
      //  Basic Inferences
      //

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

      //
      // Cell Construction
      // 

      case ERefl(e) => 
        for {
          ty <- checkI(rho, gma, e)
          v = eval(e, rho)
          res <- ty match {
            case Obj(c) => pure(Cell(c, ||(SBox(v, STree.obj(SDot(v))))))
            case Cell(c, f) => 
              for {
                st <- getFrame(f)
              } yield Cell(c, f >> SBox(v, SNode(SDot(v), st._1.asShell)))
            case _ => fail("Cannot apply reflexivity to non-cell: " + e.toString)
          }
        } yield res

      case EDrop(e) => 
        for {
          ty <- checkI(rho, gma, e)
          v = eval(e, rho)
          res <- ty match {
            case Obj(c) => pure(Cell(c, ||(SDot(v)) >> SBox(Refl(v), SLeaf)))
            case Cell(c, f) => pure(Cell(c, f >> SBox(Refl(v), SLeaf)))
            case _ => fail("Cannot apply drop to non-cell: " + e.toString)
          }
        } yield res

      case EComp(pd) => compositeType(rho, gma, pd)
      case EFill(pd) => fillType(rho, gma, pd)

      // case ELiftLeft(e, ev, c, t) => 
  //       for {
  //         pr <- inferCell(rho, gma, e)                             // Check e is a cell
  //         (cv, frm) = pr                                           // Store its frame and category
  //         ed = frm.dim                                             // Get the dimension
  //         ee = eval(e, rho)                                        // Evaluate it
  //         _ <- check(rho, gma, ev, IsLeftExt(ee))                  // Check the evidence
  //         cell = frm >> Dot(ee, S(ed))                             // Create the full cell
  //         cCell <- fromShape(cell.target)                          // Get its target
  //         cTy <- cellType(ed)(cv, cCell)                           // Extract the target type
  //         _ <- check(rho, gma, c, cTy)                             // Check c is a in that frame
  //         cVal = eval(c, rho)                                      // Evaluate c
  //         tNst <- fromShape(frm.head.replaceAt(Nil, cVal))         // Put c in the base
  //         _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))    // Check t lives in that frame
  //         tVal = eval(t, rho)                                      // Evaluate it
  //         lext <- fromShape(cell.leftExtend(cVal, Empty, tVal))    // Left extend the complex
  //         lCell <- fromShape(lext.sourceAt(Nil :: Nil))            // Find the empty lifting cell
  //         lTy <- cellType(S(ed))(cv, lCell)                        // Extract its type and we're done!
  //       } yield lTy

  //     case EFillLeft(e, ev, c, t) => 
  //       for {
  //         pr <- inferCell(rho, gma, e)
  //         (cv, frm) = pr
  //         ed = frm.dim
  //         ee = eval(e, rho)
  //         _ <- check(rho, gma, ev, IsLeftExt(ee))
  //         cell = frm >> Dot(ee, S(ed))
  //         cCell <- fromShape(cell.target)
  //         cTy <- cellType(ed)(cv, cCell)
  //         _ <- check(rho, gma, c, cTy)
  //         cVal = eval(c, rho)
  //         tNst <- fromShape(frm.head.replaceAt(Nil, cVal))
  //         _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))
  //         tVal = eval(t, rho)
  //         lext <- fromShape(cell.leftExtend(cVal, LiftLeft(ee, eval(ev, rho), cVal, tVal), tVal))
  //       } yield Cell(cv, lext)

  //     case ELiftRight(e, ev, c, t) => 
  //       for {
  //         pr <- inferCell(rho, gma, e)
  //         (cv, frm) = pr
  //         ed = frm.dim
  //         ee = eval(e, rho)
  //         a <- inferRightExt(rho, gma, ev)
  //         addr <- parseAddress(ed)(a)
  //         cell = frm >> Dot(ee, S(ed))
  //         cCell <- fromShape(frm.sourceAt(ed)(addr :: Nil))
  //         cTy <- cellType(ed)(cv, cCell)
  //         _ <- check(rho, gma, c, cTy)
  //         cVal = eval(c, rho)
  //         tNst <- fromShape(frm.head.replaceAt(addr :: Nil, cVal))
  //         _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))
  //         tVal = eval(t, rho)
  //         rext <- fromShape(cell.rightExtend(addr)(cVal, Empty, tVal))
  //         lCell <- fromShape(rext.sourceAt((addr :: Nil) :: Nil))
  //         lTy <- cellType(S(ed))(cv, lCell)
  //       } yield lTy

  //     case EFillRight(e, ev, c, t) => 
  //       for {
  //         pr <- inferCell(rho, gma, e)
  //         (cv, frm) = pr
  //         ed = frm.dim
  //         ee = eval(e, rho)
  //         a <- inferRightExt(rho, gma, ev)
  //         addr <- parseAddress(ed)(a)
  //         cell = frm >> Dot(ee, S(ed))
  //         cCell <- fromShape(frm.sourceAt(ed)(addr :: Nil))
  //         cTy <- cellType(ed)(cv, cCell)
  //         _ <- check(rho, gma, c, cTy)
  //         cVal = eval(c, rho)
  //         tNst <- fromShape(frm.head.replaceAt(addr :: Nil, cVal))
  //         _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))
  //         tVal = eval(t, rho)
  //         rext <- fromShape(cell.rightExtend(addr)(cVal, LiftRight(ee, eval(ev, rho), cVal, tVal), tVal))
  //       } yield Cell(cv, rext)

  //     //
  //     //  Property Inferences
  //     //

  //     case EDropIsLeft(c, e) => 
  //       for {
  //         _ <- checkI(rho, gma, EDrop(c, e))
  //         ef = eval(EDrop(c, e), rho)
  //       } yield IsLeftExt(ef)

  //     case EFillIsLeft(c, d, pd) => 
  //       for {
  //         _ <- checkI(rho, gma, EFill(c, d, pd))     // Infer that the fill is well-formed
  //         ef = eval(EFill(c, d, pd), rho)            // Evaluate it ...
  //       } yield IsLeftExt(ef)                        // and we know the type!

  //     case EShellIsLeft(e, ev, s, t) => 
  //       for {
  //         pr <- inferCell(rho, gma, e)
  //         (cv, frm) = pr
  //         ee = eval(e, rho)
  //         ed = frm.dim
  //         _ <- check(rho, gma, ev, IsLeftExt(ee))
  //         eTgt = frm.head.baseValue
  //         eSrc <- sourceTree(frm)
  //         srcTr <- parseTree(ed)(s)
  //         oTr <- fromShape(
  //           Tree.matchTraverse[Val, Expr, Option[Val], Nat](eSrc, srcTr)({
  //             case (u, EEmpty) => succeed(Some(u))
  //             case (u, v) => 
  //               for { 
  //                 _ <- toShape(check(rho, gma, v, IsLeftExt(u))) 
  //               } yield None
  //           })
  //         )
  //         ty <- (
  //           (oTr.nodes.filter(_.isDefined), t) match {
  //             case (Nil, EEmpty) => pure(IsLeftExt(eTgt))
  //             case (Some(u) :: Nil, tev) => 
  //               for {
  //                 _ <- check(rho, gma, tev, IsLeftExt(eTgt))
  //               } yield IsLeftExt(u)
  //             case _ => fail("Malformed shell evidence")
  //           }
  //         )
  //       } yield ty

  //     case EFillLeftIsLeft(e, ev, c, t) => 
  //       for {
  //         pr <- inferCell(rho, gma, e)
  //         (cv, frm) = pr
  //         ee = eval(e, rho)
  //         ed = frm.dim
  //         _ <- check(rho, gma, ev, IsLeftExt(ee))
  //         cell = frm >> Dot(ee, S(ed))
  //         cCell <- fromShape(cell.target)
  //         cTy <- cellType(ed)(cv, cCell)
  //         _ <- check(rho, gma, c, cTy)
  //         cVal = eval(c, rho)
  //         tNst <- fromShape(frm.head.replaceAt(Nil, cVal))
  //         _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))
  //         tVal = eval(t, rho)
  //       } yield IsLeftExt(FillLeft(ee, eval(ev, rho), cVal, tVal))

  //     case EFillRightIsLeft(e, ev, c, t) => 
  //       for {
  //         pr <- inferCell(rho, gma, e)
  //         (cv, frm) = pr
  //         ed = frm.dim
  //         ee = eval(e, rho)
  //         a <- inferRightExt(rho, gma, ev)
  //         addr <- parseAddress(ed)(a)
  //         cell = frm >> Dot(ee, S(ed))
  //         cCell <- fromShape(frm.sourceAt(ed)(addr :: Nil))
  //         cTy <- cellType(ed)(cv, cCell)
  //         _ <- check(rho, gma, c, cTy)
  //         cVal = eval(c, rho)
  //         tNst <- fromShape(frm.head.replaceAt(addr :: Nil, cVal))
  //         _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))
  //         tVal = eval(t, rho)
  //       } yield IsLeftExt(FillRight(ee, eval(ev, rho), cVal, tVal))

  //     case EFillLeftIsRight(e, ev, c, t, l, f, fev) => 
  //       for {
  //         pr <- inferCell(rho, gma, e)                           
  //         (cv, frm) = pr                                         
  //         ed = frm.dim                                           
  //         ee = eval(e, rho)                                      
  //         _ <- check(rho, gma, ev, IsLeftExt(ee))                
  //         cell = frm >> Dot(ee, S(ed))                           
  //         cCell <- fromShape(cell.target)                        
  //         cTy <- cellType(ed)(cv, cCell)                         
  //         _ <- check(rho, gma, c, cTy)                           
  //         cVal = eval(c, rho)                                    
  //         tNst <- fromShape(frm.head.replaceAt(Nil, cVal))       
  //         _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))  
  //         tVal = eval(t, rho)                                    
  //         lext <- fromShape(cell.leftExtend(cVal, Empty, tVal))  
  //         lCell <- fromShape(lext.sourceAt(Nil :: Nil))          
  //         lTy <- cellType(S(ed))(cv, lCell)                      
  //         _ <- check(rho, gma, l, lTy)
  //         lVal = eval(l, rho)
  //         lNst <- fromShape(lext.head.replaceAt(Nil :: Nil, lVal))
  //         _ <- check(rho, gma, f, Cell(cv, lext.withHead(lNst)))
  //         fVal = eval(f, rho)
  //         _ <- check(rho, gma, fev, IsLeftExt(fVal))
  //       } yield IsRightExt(fVal, ANil)

  //     case EFillRightIsRight(e, ev, c, t, l, f, fev) => 
  //       for {
  //         pr <- inferCell(rho, gma, e)
  //         (cv, frm) = pr
  //         ed = frm.dim
  //         ee = eval(e, rho)
  //         a <- inferRightExt(rho, gma, ev)
  //         addr <- parseAddress(ed)(a)
  //         cell = frm >> Dot(ee, S(ed))
  //         cCell <- fromShape(frm.sourceAt(ed)(addr :: Nil))
  //         cTy <- cellType(ed)(cv, cCell)
  //         _ <- check(rho, gma, c, cTy)
  //         cVal = eval(c, rho)
  //         tNst <- fromShape(frm.head.replaceAt(addr :: Nil, cVal))
  //         _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))
  //         tVal = eval(t, rho)
  //         rext <- fromShape(cell.rightExtend(addr)(cVal, Empty, tVal))
  //         lCell <- fromShape(rext.sourceAt((addr :: Nil) :: Nil))
  //         lTy <- cellType(S(ed))(cv, lCell)
  //         _ <- check(rho, gma, l, lTy)
  //         lVal = eval(l, rho)
  //         lNst <- fromShape(rext.head.replaceAt((addr :: Nil) :: Nil, lVal))
  //         _ <- check(rho, gma, f, Cell(cv, rext.withHead(lNst)))
  //         fVal = eval(f, rho)
  //         _ <- check(rho, gma, fev, IsLeftExt(fVal))
  //       } yield IsRightExt(fVal, rbAddr(S(ed))(addr :: Nil))

      // Oh crap ....
      case e => fail("checkI: " ++ e.toString)

    }

  }

}
