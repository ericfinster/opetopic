/**
  * TypeChecker.scala - TypeChecker for MiniTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.newtt

import opetopic._
import syntax.tree._
import TypeLemmas._

object OTTTypeChecker {

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
      case ECat => Cat
      case EOb(c) => Ob(eval(c, rho))
      case ECell(c, d, s, t) => {
        val srcTr = getOrError(parseExprTree(d)(s))
        Cell(eval(c, rho), d, srcTr.map(eval(_, rho)), eval(t, rho))
      }
      case EIsLeftExt(e) => IsLeftExt(eval(e, rho))
      case EIsRightExt(e, a) => IsRightExt(eval(e, rho), a)
      case ERefl(c, e) => Refl(eval(c, rho), eval(e, rho))
      case EDrop(c, e) => Drop(eval(c, rho), eval(e, rho))
      case EComp(c, pd) => Comp(eval(c, rho), eval(pd, rho))
      case EFill(c, pd) => Fill(eval(c, rho), eval(pd, rho))
      // These should be errors: eliminate tree values
      case ELf => error("Unreduced leaf")
      case EPt(e) => error("Unreduced point")
      case ENd(e, s) => error("Unreduced node")
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
      case Ob(v) => EOb(rbV(i, v))
      case Cell(c, d, s, t) => ECell(rbV(i, c), d, treeToExpr(d)(s.map(rbV(i, _))), rbV(i, t))
      case IsLeftExt(v) => EIsLeftExt(rbV(i, v))
      case IsRightExt(v, a) => EIsRightExt(rbV(i, v), a)
      case Refl(c, v) => ERefl(rbV(i, c), rbV(i, v))
      case Drop(c, v) => EDrop(rbV(i, c), rbV(i, v))
      case Comp(c, pd) => EComp(rbV(i, c), rbV(i, pd))
      case Fill(c, pd) => EFill(rbV(i, c), rbV(i, pd))
      case Nt(k) => rbN(i, k)
      // I think these should be errors and you should eliminate tree values ...
      // case VLf => ELf
      // case VPt(v) => EPt(rbV(i, v))
      // case VNd(v, s) => ENd(rbV(i, v), rbV(i, s))
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

  import scalaz.\/
  import scalaz.-\/
  import scalaz.\/-
  import scalaz.Monad
  import scalaz.std.list._

  type ErrorMessage = String
  type G[A] = \/[ErrorMessage, A]

  val M = Monad[G]
  import M._

  def getOrError[A](m : G[A]) : A = 
    m match {
      case -\/(msg) => throw new IllegalStateException(msg)
      case \/-(a) => a
    }

  def fail[A](str: String) : G[A] = 
    -\/(str)

  def toShape[A](m: G[A]) : ShapeM[A] = 
    m match {
      case -\/(msg) => -\/(ShapeError(msg))
      case \/-(a) => \/-(a)
    }

  def fromShape[A](m: ShapeM[A]) : G[A] = 
    m match {
      case -\/(ShapeError(msg)) => -\/(msg)
      case \/-(a) => \/-(a)
    }

  def fromOpt[A](o: Option[A], msg: String) : G[A] = 
    o match {
      case None => fail(msg)
      case Some(a) => \/-(a)
    }

  //============================================================================================
  // TREE AND COMPLEX ROUTINES
  //

  @natElim
  def parseExprTree[N <: Nat](n: N)(e: Expr) : G[Tree[Expr, N]] = {
    case (Z, EPt(e)) => pure(Pt(e))
    case (Z, e) => fail("Not a tree expression in dim 0: " + e.toString)
    case (S(p: P), ELf) => pure(Leaf(S(p)))
    case (S(p: P), ENd(e, sh)) => 
      for {
        shParse <- parseExprTree(p)(sh)
        shRes <- shParse.traverse(parseExprTree(S(p))(_))
      } yield Node(e, shRes)
    case (S(p: P), e) => fail("Not a tree expression: " + e.toString)
  }

  @natElim
  def treeToExpr[N <: Nat](n: N)(t: Tree[Expr, N]) : Expr = {
    case (Z, Pt(e)) => EPt(e)
    case (S(p), Leaf(_)) => ELf
    case (S(p), Node(e, sh)) => ENd(e, treeToExpr(p)(sh.map(treeToExpr(S(p))(_))))
  }


  // Don't think we need this now ... seems we should try to keep the
  // values as semantic trees ...
  // @natElim
  // def parseValTree[N <: Nat](n: N)(v: Val) : G[Tree[Val, N]] = {
  //   case (Z, VPt(v)) => pure(Pt(v))
  //   case (Z, v) => fail("Not a tree value in dim 0: " + v.toString)
  //   case (S(p: P), VLf) => pure(Leaf(S(p)))
  //   case (S(p: P), VNd(v, s)) => 
  //     for {
  //       shParse <- parseValTree(p)(s)
  //       shRes <- shParse.traverse(parseValTree(S(p))(_))
  //     } yield Node(v, shRes)
  //   case (S(p: P), v) => fail("Not a tree value: " + v.toString)
  // }

  // @natElim
  // def treeToVal[N <: Nat](n: N)(t: Tree[Val, N]) : Val = {
  //   case (Z, Pt(v)) => VPt(v)
  //   case (S(p), Leaf(_)) => VLf
  //   case (S(p), Node(v, sh)) => VNd(v, treeToVal(p)(sh.map(treeToVal(S(p))(_))))
  // }

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

  // def buildWeb[P <: Nat](p: P)(cv: Val, bv: Val)(rho: Rho, gma: Gamma, tr: Tree[Expr, S[P]]) : G[Nesting[Val, P]] = 
  //   tr match {
  //     case Leaf(_) => pure(Nesting.external(p)(bv))
  //     case Node(e, sh) => 
  //       for {
  //         frmV <- checkI(rho, gma, e)
  //         l = lRho(rho)
  //         frm <- (
  //           frmV match {
  //             case Cell(c, _, st, tv) => 
  //               for {
  //                 ct <- parseValTree(p)(st)
  //                 _ <- eqNf(l, cv, c)  // Check that it's in the right category
  //                 _ <- eqNf(l, tv, bv) // Check that the target is the incoming value specified ...
  //                 cn <- fromShape(
  //                   Tree.matchTraverse(ct, sh)({
  //                     case (sv, et) => toShape(buildWeb(p)(c, sv)(rho, gma, et))
  //                   })
  //                 )
  //               } yield Box(tv, cn)
  //             case _ => fail("Expresion is not a cell: " + e.toString)
  //           }
  //         )
  //       } yield frm
  //   }

  // This could be easily modified to return the whole we at once.  Not sure if
  // I'll need that ....
  def checkWeb[P <: Nat](p: P)(rho: Rho, gma: Gamma)(cv: Val, lvs: Tree[Val, P], tr: Tree[Expr, S[P]]) : G[Val] = 
    fromShape(
      Tree.graftRec[Expr, Val, P](p)(tr)(
        (a: Address[P]) => Tree.valueAt(lvs, a)
      )({
        case (e, cn) => toShape(
          for {
            frmV <- checkI(rho, gma, e)
            l = lRho(rho)
            rv <- (
              frmV match {
                case Cell(c, d, st, tv) =>
                for {
                  _ <- eqNf(l, cv, c)  // Check that it's in the right category
                  neq <- fromOpt(matchNatPair(d, p), "Wrong dimension")
                  _ <- fromShape(
                    Tree.matchTraverse(rewriteNatIn[ValTree, Nat, P](neq)(st), cn)({
                      case (sv, iv) => toShape(eqNf(l, sv, iv)) // Check the source value is the input value
                    })
                  )
                } yield tv // Give back the output type value
                case _ => fail("Expression is not a cell: " + e.toString)
              }
            )
          } yield rv
        )
      })
    )

  @natElim
  def checkFrame[N <: Nat](n: N)(c: Expr, s: Expr, t: Expr)(rho: Rho, gma: Gamma) : G[Unit] = {
    case (Z, c, EPt(s), t, rho, gma) => 
      for {
        _ <- check(rho, gma, c, Cat)
        cv = eval(c, rho)
        _ <- check(rho, gma, s, Ob(cv))
        _ <- check(rho, gma, t, Ob(cv))
      } yield ()
    case (Z, c, _, t, rho, gma) => fail("Not an object in dimension 0")
    case (S(p: P), c, s, t, rho, gma) => 
      for {
        _ <- check(rho, gma, c, Cat)
        cv = eval(c, rho)
        tcv <- checkI(rho, gma, t)
        l = lRho(rho)
        _ <- (
          tcv match {
            case Cell(tcat, d, ts, tt) => 
              for {
                _ <- eqNf(l, cv, tcat)                                   // Target lives in the right category
                neq <- fromOpt(matchNatPair(d, p), "Wrong dimension")    // Ensure dimensions match
                ttr = rewriteNatIn[ValTree, Nat, P](neq)(ts)             // Cast to correct dimension
                srcTr <- parseExprTree(S(p))(s)                          // Parse the source tree
                ov <- checkWeb(p)(rho, gma)(cv, ttr, srcTr)              // Recursively verify the source tree
                _ <- eqNf(l, ov, tt)                                     // Target type matches the output
              } yield ()
            case _ => fail("Target " + t.toString + " is not a cell")
          }
        )
      } yield ()
  }

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
      case (EOb(c), Type) =>
        for {
          _ <- check(rho, gma, c, Cat)
        } yield ()
      case (ECell(c, d, s, t), Type) => 
        checkFrame(d)(c, s, t)(rho, gma)
      case (EIsLeftExt(e), Type) => 
        for {
          t <- checkI(rho, gma, e)
          _ <- t match {
            case Cell(_, _, _, _) => pure(())
            case _ => fail("Expression " + e.toString + " is not a cell")
          }
        } yield ()
      case (EIsRightExt(e, a), Type) => 
        for {
          t <- checkI(rho, gma, e)
          _ <- t match {
            case Cell(_, d, s, t) => 
              for {
                addr <- parseAddress(d)(a)
                _ <- fromShape(s.seekTo(addr))  // Seek to the given address to make sure it's valid
              } yield ()
            case _ => fail("Expression " + e.toString + " is not a cell")
          }
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
      case ERefl(c, e) => 
        for {
          _ <- check(rho, gma, c, Cat)
          cv = eval(c, rho)
          ct <- checkI(rho, gma, e)
          res <- ct match {
            case Ob(ec) => 
              for {
                _ <- eqNf(lRho(rho), cv, ec)
                ee = eval(e, rho)
              } yield Cell(ec, Z, Pt(ee), ee)
            case Cell(ec, ed, es, et) => 
              for {
                _ <- eqNf(lRho(rho), cv, ec)
                ee = eval(e, rho)
                cor = Node(ee, es.map(_ => Leaf(S(ed))))
              } yield Cell(ec, S(ed), cor, ee)  
            case _ => fail("Expression " + e.toString + " is not a cell")
          }
        } yield res
      case EDrop(c, e) => 
        for {
          _ <- check(rho, gma, c, Cat)
          cv = eval(c, rho)
          ct <- checkI(rho, gma, e)
          res <- ct match {
            case Ob(ec) => 
              for {
                _ <- eqNf(lRho(rho), cv, ec)
                ee = eval(e, rho)
              } yield Cell(ec, S(Z), Leaf(S(Z)), Refl(cv, ee))
            case Cell(ec, ed, es, et) => 
              for {
                _ <- eqNf(lRho(rho), cv, ec)
                ee = eval(e, rho)
              } yield Cell(ec, S(S(ed)), Leaf(S(S(ed))), Refl(cv, ee))  
            case _ => fail("Expression " + e.toString + " is not a cell")
          }
        } yield res
      case EComp(c, pd) => 
        for {
          _ <- check(rho, gma, c, Cat)
          cv = eval(c, rho)

          // So, here we are going to need to
          // parse the tree (which means we need a dimension)
          // and then calculate the corresponding boundary type.


        } yield ???
      case e => fail("checkI: " ++ e.toString)
    }

  }

}
