/**
  * TypeChecker.scala - TypeChecker for MiniTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import opetopic._
import syntax.tree._
import syntax.nesting._
import syntax.complex._
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

      // Categories and Cells
      case ECat => Cat
      case EOb(c) => Ob(eval(c, rho))
      case ECell(c, e) => {
        val fc = getOrError(parseComplex(e))
        Cell(eval(c, rho), fc.value.map(EvalMap(rho)))
      }

      // Propertes
      case EIsLeftExt(e) => IsLeftExt(eval(e, rho))
      case EIsRightExt(e, a) => IsRightExt(eval(e, rho), a)

      // Composition and identities
      case ERefl(c, e) => Refl(eval(c, rho), eval(e, rho))
      case EDrop(c, e) => Drop(eval(c, rho), eval(e, rho))
      case EComp(c, d, pd) => {
        val dim = intToNat(d)
        val pdTr = getOrError(parseTree(dim)(pd))
        Comp(eval(c, rho), dim, pdTr.map(eval(_, rho)))
      }
      case EFill(c, d, pd) => {
        val dim = intToNat(d)
        val pdTr = getOrError(parseTree(dim)(pd))
        Fill(eval(c, rho), dim, pdTr.map(eval(_, rho)))
      }

      // Liftings
      case ELiftLeft(e, ev, c, t) => LiftLeft(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case EFillLeft(e, ev, c, t) => FillLeft(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case ELiftRight(e, ev, c, t) => LiftRight(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case EFillRight(e, ev, c, t) => FillRight(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))

      // Base properties
      case EFillIsLeft(c, d, pd) => {
        val dim = intToNat(d)
        val pdTr = getOrError(parseTree(dim)(pd))
        FillIsLeft(eval(c, rho), dim, pdTr.map(eval(_, rho)))
      }
      case EDropIsLeft(c, e) => DropIsLeft(eval(c, rho), eval(e, rho))
      case EShellIsLeft(e, ev, s, t) => ShellIsLeft(eval(e, rho), eval(ev, rho), eval(s, rho), eval(t, rho))

      // Derived properties
      case EFillLeftIsLeft(e, ev, c, t) => FillLeftIsLeft(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case EFillRightIsLeft(e, ev, c, t) => FillRightIsLeft(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho))
      case EFillLeftIsRight(e, ev, c, t, l, f, fev) => 
        FillLeftIsRight(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho), eval(l, rho), eval(f, rho), eval(fev, rho))
      case EFillRightIsRight(e, ev, c, t, l, f, fev) => 
        FillRightIsRight(eval(e, rho), eval(ev, rho), eval(c, rho), eval(t, rho), eval(l, rho), eval(f, rho), eval(fev, rho))

      // Trees reduce ...
      case ELf => VLf
      case EPt(e) => VPt(eval(e, rho)) 
      case ENd(e, sh) => VNd(eval(e, rho), eval(sh, rho))

      // ... Nestings and complexes don't.
      case EDot(_) => error("Unreduced dot")
      case EBox(_, _) => error("Unreduced box")
      case EHd(_) => error("Unreduced head")
      case ETl(_, _) => error("Unreduced tail")
    }

  case class EvalMap(rho: Rho) extends IndexedMap[ConstExpr, ConstVal] {
    def apply[N <: Nat](n: N)(e: Expr) = eval(e, rho)
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
      case Cell(c, frm) => ECell(rbV(i, c), complexToExpr(frm.dim)(frm.map(RbMap(i))))

      case IsLeftExt(v) => EIsLeftExt(rbV(i, v))
      case IsRightExt(v, a) => EIsRightExt(rbV(i, v), a)

      case Refl(c, v) => ERefl(rbV(i, c), rbV(i, v))
      case Drop(c, v) => EDrop(rbV(i, c), rbV(i, v))
      case Comp(c, d, pd) => EComp(rbV(i, c), natToInt(d), treeToExpr(d)(pd.map(rbV(i, _))))
      case Fill(c, d, pd) => EFill(rbV(i, c), natToInt(d), treeToExpr(d)(pd.map(rbV(i, _))))

      case LiftLeft(e, ev, c, t) => ELiftLeft(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))
      case FillLeft(e, ev, c, t) => EFillLeft(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))
      case LiftRight(e, ev, c, t) => ELiftRight(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))
      case FillRight(e, ev, c, t) => EFillRight(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))

      case DropIsLeft(c, v) => EDropIsLeft(rbV(i, c), rbV(i, v))
      case FillIsLeft(c, d, pd) => EFillIsLeft(rbV(i, c), natToInt(d), treeToExpr(d)(pd.map(rbV(i, _))))
      case ShellIsLeft(e, ev, s, t) => EShellIsLeft(rbV(i, e), rbV(i, ev), rbV(i, s), rbV(i, t))

      case FillLeftIsLeft(e, ev, c, t) => EFillLeftIsLeft(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))
      case FillRightIsLeft(e, ev, c, t) => EFillRightIsLeft(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t))
      case FillLeftIsRight(e, ev, c, t, l, f, fev) => EFillLeftIsRight(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t), rbV(i, l), rbV(i, f), rbV(i, fev))
      case FillRightIsRight(e, ev, c, t, l, f, fev) => EFillRightIsRight(rbV(i, e), rbV(i, ev), rbV(i, c), rbV(i, t), rbV(i, l), rbV(i, f), rbV(i, fev))

      case VLf => ELf
      case VPt(v) => EPt(rbV(i, v))
      case VNd(v, sh) => ENd(rbV(i, v), rbV(i, sh))

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

  case class RbMap(i: Int) extends IndexedMap[ConstVal, ConstExpr] {
    def apply[N <: Nat](n: N)(v: Val) = rbV(i, v)
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
  def parseTree[N <: Nat](n: N)(e: Expr) : G[Tree[Expr, N]] = {
    case (Z, EPt(e)) => pure(Pt(e))
    case (Z, e) => fail("Not a tree expression in dim 0: " + e.toString)
    case (S(p: P), ELf) => pure(Leaf(S(p)))
    case (S(p: P), ENd(e, sh)) => 
      for {
        shParse <- parseTree(p)(sh)
        shRes <- shParse.traverse(parseTree(S(p))(_))
      } yield Node(e, shRes)
    case (S(p: P), e) => fail("Not a tree expression: " + e.toString)
  }

  @natElim
  def parseNesting[N <: Nat](n: N)(e: Expr) : G[Nesting[Expr, N]] = {
    case (Z, EDot(e)) => pure(Obj(e))
    case (Z, EBox(e, ec)) => 
      for {
        t <- parseTree(Z)(ec)
        cn <- t.traverse[G, Nesting[Expr, _0]](parseNesting(Z)(_))
      } yield Box(e, cn)
    case (Z, _) => fail("Invalid nesting")
    case (S(p: P), EDot(e)) => pure(Dot(e, S(p)))
    case (S(p: P), EBox(e, ec)) => 
      for {
        t <- parseTree(S(p))(ec)
        cn <- t.traverse[G, Nesting[Expr, S[P]]](parseNesting(S(p))(_))
      } yield Box(e, cn)
    case (S(p: P), _) => fail("Invalid nesting")
  }

  def parseComplex[N <: Nat](n: N)(e: Expr, s: Suite[ExprNesting, N]) : G[FiniteComplex[ConstExpr]] = 
    e match {
      case EHd(ne) => 
        for {
          nst <- parseNesting(n)(ne)
        } yield Sigma[ExprComplex, N](n)(s >> nst)
      case ETl(hd, tl) => 
        for {
          nst <- parseNesting(n)(hd)
          res <- parseComplex(S(n))(tl, s >> nst)
        } yield res
      case _ => fail("Invalid complex")
    }

  def parseComplex(e: Expr) : G[FiniteComplex[ConstExpr]] = 
    parseComplex(Z)(e, SNil[ExprNesting]())

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

  @natElim
  def treeToExpr[N <: Nat](n: N)(t: Tree[Expr, N]) : Expr = {
    case (Z, Pt(e)) => EPt(e)
    case (S(p), Leaf(_)) => ELf
    case (S(p), Node(e, sh)) => ENd(e, treeToExpr(p)(sh.map(treeToExpr(S(p))(_))))
  }

  def nestingToExpr[N <: Nat](n: N)(nst: Nesting[Expr, N]) : Expr = 
    nst match {
      case Obj(e) => EDot(e)
      case Dot(e, _) => EDot(e)
      case Box(e, cn) => EBox(e, treeToExpr(n)(cn.map(nestingToExpr(n)(_))))
    }

  @natElim
  def complexToExpr[N <: Nat](n: N)(c: ExprComplex[N], tlOpt: Option[Expr] = None) : Expr = {
    case (Z, Complex(_, hd), tlOpt) => {
      val hde = nestingToExpr(Z)(hd)
      tlOpt.map(ETl(hde, _)) getOrElse EHd(hde)
    }
    case (S(p), Complex(tl, hd), tlOpt) => {
      val hde = nestingToExpr(S(p))(hd)
      complexToExpr(p)(tl, Some(tlOpt.map(ETl(hde, _)).getOrElse(EHd(hde))))
    }
  }

  def sourceTree[A[_ <: Nat], N <: Nat](c: Complex[A, N]) : G[Tree[A[N], N]] = 
    c.head match {
      case Box(_, cn) => 
        for {
          st <- cn.traverse[G, A[N]]((nst : Nesting[A[N], N]) => 
            if (Nesting.isExternal(nst)) 
              pure(nst.baseValue) 
            else 
              fail("Source tree contains non-external nesting")
          )
        } yield st
      case _ => fail("Error extracting source tree")
    }

  @natElim
  def checkCell[N <: Nat](n: N)(rho: Rho, gma: Gamma, cmplx: ExprComplex[N], cat: Val) : G[Unit] = {
    case (Z, rho, gma, Complex(_, Obj(e)), cat) => check(rho, gma, e, Ob(cat))
    case (Z, rho, gma, Complex(_, _), cat) => fail("checkCell: too many objects!")
    case (S(p: P), rho, gma, Complex(tl, Dot(e, _)), cat) => {
      for {
        _ <- checkFrame(p)(rho, gma, tl, cat)
        _ <- check(rho, gma, e, Cell(cat, tl.map(EvalMap(rho))))
      } yield ()
    }
    case (S(p: P), rho, gma, Complex(tl, _), cat) => fail("checkCell: too many top cells!")
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
    case (S(p: P), rho, gma, Complex(Complex(tl, web), frm @ Box(t, cn)), cat) => 
      for {
        _ <- fromShape( // The following match traverse should ensure that the complex is well-shaped
          Tree.matchWithAddress(S(p))(web.toTree, cn)({
            case (expr, nst, addr) => toShape(
              if (! Nesting.isExternal(nst)) 
                fail("Frame has non-external nesting in its canopy")
              else 
                for {
                  face <- fromShape(cmplx.sourceAt(S(p))(addr :: Nil))
                  _ <- checkCell(S(p))(rho, gma, face, cat)
                } yield ()
            )
          })
        )
        tc <- fromShape(cmplx.sourceAt(S(p))(Nil))  // Now check the target
        _ <- checkCell(S(p))(rho, gma, tc, cat)
      } yield ()
    case (S(p: P), rho, gma, _, cat) => fail("Malformed complex")
  }

  def inferCell(rho: Rho, gma: Gamma, e: Expr) : G[(Val, ValComplex[Nat])] = 
    for {
      et <- checkI(rho, gma, e)
      res <- et match {
        case Cell(c, f) => pure((c, f))
        case _ => fail("Expression is not a cell: " + e.toString)
      }
    } yield res

  def inferObject(rho: Rho, gma: Gamma, e: Expr) : G[Val] = 
    for {
      et <- checkI(rho, gma, e)
      res <- et match {
        case Ob(cv) => pure(cv)
        case _ => fail("Expression is not an object: " + e.toString)
      }
    } yield res

  def inferRightExt(rho: Rho, gma: Gamma, e: Expr) : G[Addr] = 
    for {
      rev <- checkI(rho, gma, e)
      a <- rev match {
        case IsRightExt(_, ra) => pure(ra)
        case _ => fail("Evidence is not for a right extension")
      }
    } yield a

  def nfDiscriminator(rho : Rho) : Discriminator[ConstVal] = 
    new Discriminator[ConstVal] {
      val l = lRho(rho)
      def apply[N <: Nat](n: N)(u: Val, v: Val) = 
        toShape(for { _ <- eqNf(l, u, v) } yield u)
    }

  @natElim 
  def cellType[N <: Nat](n: N)(cv: Val, c: ValComplex[N]) : G[Val] = {
    case (Z, cv, Complex(_, Obj(_))) => pure(Ob(cv))
    case (Z, cv, _) => fail("Not an object")
    case (S(p), cv, Complex(tl, Dot(_, _))) => pure(Cell(cv, tl))
    case (S(p), cv, _) => fail("Not a cell")
  }

  @natElim
  def compositeType[D <: Nat](d: D)(rho: Rho, gma: Gamma, cv: Val, tr: Tree[Expr, D]) : G[Val] = {
    case (Z, rho, gma, cv, Pt(e)) => 
      for {
        et <- inferObject(rho, gma, e)
        _ <- eqNf(lRho(rho), et, cv)
      } yield Ob(cv)
    case (S(p: P), rho, gma, cv, Leaf(_)) => fail("Cannot compose leaf, use refl")
    case (S(p: P), rho, gma, cv, tr) => 
      for {
        pr <- buildComplex(p)(rho, gma)(cv, tr)
        (tl, pd) = pr
        fc = tl >> Box(Empty, pd)
        cc <- fromShape(fc.sourceAt(S(p))(Nil))
        ct <- cellType(S(p))(cv, cc)
      } yield ct
    }

  @natElim
  def fillType[N <: Nat](n: N)(rho: Rho, gma: Gamma, cv: Val, tr: Tree[Expr, N]) : G[Val] = {
    case (Z, rho, gma, cv, Pt(e)) => 
      for {
        et <- inferObject(rho, gma, e)
        _ <- eqNf(lRho(rho), et, cv)
        ee = eval(e, rho)
      } yield Cell(cv, Complex[ConstVal] >> Box(Comp(cv, Z, Pt(ee)), Pt(Obj(ee))))
    case (S(p: P), rho, gma, cv, Leaf(_)) => fail("Cannot fill leaf, use drop")
    case (S(p: P), rho, gma, cv, tr) => 
      for {
        pr <- buildComplex(p)(rho, gma)(cv, tr)
        (tl, pd) = pr
      } yield Cell(cv, tl >> Box(Comp(cv, S(p), tr.map(eval(_, rho))), pd))
  }

  // I believe this assumes that the tree is not a leaf.
  def buildComplex[P <: Nat](p: P)(rho: Rho, gma: Gamma)(cv: Val, tr: Tree[Expr, S[P]]) 
      : G[(ValComplex[P], Tree[Nesting[Val, S[P]], S[P]])] = 
    for {
      pd <- tr.traverse[G, ValComplex[S[P]]]((e: Expr) => {
        for {
          tp <- inferCell(rho, gma, e)
          (ec, ef) = tp
          ed = ef.dim
          ee = eval(e, rho)
          _ <- eqNf(lRho(rho), cv, ec)
          eqEv <- fromOpt(matchNatPair(ed, p), "Wrong dimension")
        } yield rewriteNatIn[ValComplex, Nat, P](eqEv)(ef) >> Dot(ee, S(p))
      })
      res <- fromShape(
        Complex.paste(p)(pd)(nfDiscriminator(rho))
      )
    } yield res

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
      case (ECell(c, e), Type) => 
        for {
          _ <- check(rho, gma, c, Cat)
          cv = eval(c, rho)
          fc <- parseComplex(e)
          _ <- checkFrame(fc.n)(rho, gma, fc.value, cv) 
        } yield ()
      case (EIsLeftExt(e), Type) => 
        for {
          _ <- inferCell(rho, gma, e)
        } yield ()
      case (EIsRightExt(e, a), Type) => 
        for {
          pr <- inferCell(rho, gma, e)
          (cv, frm) = pr
          d = frm.dim
          addr <- parseAddress(d)(a)
          _ <- fromShape(frm.head.seekTo(addr :: Nil))
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
              } yield Cell(ec, Complex[ConstVal] >> Box(ee, Pt(Obj(ee))))
            case Cell(ec, efrm) => 
              for {
                _ <- eqNf(lRho(rho), cv, ec)
                ee = eval(e, rho)
                d = efrm.dim
                srcTr <- sourceTree(efrm)
              } yield Cell(ec, efrm >> Box(ee, Node(Dot(ee, S(d)), srcTr.map(_ => Leaf(S(d))))))
            case _ => fail("Expression " + e.toString + " is not a cell or object")
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
              } yield Cell(ec, Complex[ConstVal] >> Obj(ee) >> Box(Refl(ec, ee), Leaf(S(Z)))) 
            case Cell(ec, efrm) => 
              for {
                _ <- eqNf(lRho(rho), cv, ec)
                ee = eval(e, rho)
                d = efrm.dim
                srcTr <- sourceTree(efrm)
              } yield Cell(ec, efrm >> Dot(ee, S(d)) >> Box(Refl(ec, ee), Leaf(S(S(d)))))
            case _ => fail("Expression " + e.toString + " is not a cell or object")
          }
        } yield res

      case EComp(c, d, pd) => 
        for {
          _ <- check(rho, gma, c, Cat)
          cv = eval(c, rho)
          dim = intToNat(d)
          pdTr <- parseTree(dim)(pd)
          res <- compositeType(dim)(rho, gma, cv, pdTr)
        } yield res

      case EFill(c, d, pd) => 
        for {
          _ <- check(rho, gma, c, Cat)
          cv = eval(c, rho)
          dim = intToNat(d)
          pdTr <- parseTree(dim)(pd)
          res <- fillType(dim)(rho, gma, cv, pdTr)
        } yield res

      case ELiftLeft(e, ev, c, t) => 
        for {
          pr <- inferCell(rho, gma, e)                             // Check e is a cell
          (cv, frm) = pr                                           // Store its frame and category
          ed = frm.dim                                             // Get the dimension
          ee = eval(e, rho)                                        // Evaluate it
          _ <- check(rho, gma, ev, IsLeftExt(ee))                  // Check the evidence
          cell = frm >> Dot(ee, S(ed))                             // Create the full cell
          cCell <- fromShape(cell.target)                          // Get its target
          cTy <- cellType(ed)(cv, cCell)                           // Extract the target type
          _ <- check(rho, gma, c, cTy)                             // Check c is a in that frame
          cVal = eval(c, rho)                                      // Evaluate c
          tNst <- fromShape(frm.head.replaceAt(Nil, cVal))         // Put c in the base
          _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))    // Check t lives in that frame
          tVal = eval(t, rho)                                      // Evaluate it
          lext <- fromShape(cell.leftExtend(cVal, Empty, tVal))    // Left extend the complex
          lCell <- fromShape(lext.sourceAt(Nil :: Nil))            // Find the empty lifting cell
          lTy <- cellType(S(ed))(cv, lCell)                        // Extract its type and we're done!
        } yield lTy

      case EFillLeft(e, ev, c, t) => 
        for {
          pr <- inferCell(rho, gma, e)
          (cv, frm) = pr
          ed = frm.dim
          ee = eval(e, rho)
          _ <- check(rho, gma, ev, IsLeftExt(ee))
          cell = frm >> Dot(ee, S(ed))
          cCell <- fromShape(cell.target)
          cTy <- cellType(ed)(cv, cCell)
          _ <- check(rho, gma, c, cTy)
          cVal = eval(c, rho)
          tNst <- fromShape(frm.head.replaceAt(Nil, cVal))
          _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))
          tVal = eval(t, rho)
          lext <- fromShape(cell.leftExtend(cVal, LiftLeft(ee, eval(ev, rho), cVal, tVal), tVal))
        } yield Cell(cv, lext)

      case ELiftRight(e, ev, c, t) => 
        for {
          pr <- inferCell(rho, gma, e)
          (cv, frm) = pr
          ed = frm.dim
          ee = eval(e, rho)
          a <- inferRightExt(rho, gma, ev)
          addr <- parseAddress(ed)(a)
          cell = frm >> Dot(ee, S(ed))
          cCell <- fromShape(frm.sourceAt(ed)(addr :: Nil))
          cTy <- cellType(ed)(cv, cCell)
          _ <- check(rho, gma, c, cTy)
          cVal = eval(c, rho)
          tNst <- fromShape(frm.head.replaceAt(addr :: Nil, cVal))
          _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))
          tVal = eval(t, rho)
          rext <- fromShape(cell.rightExtend(addr)(cVal, Empty, tVal))
          lCell <- fromShape(rext.sourceAt((addr :: Nil) :: Nil))
          lTy <- cellType(S(ed))(cv, lCell)
        } yield lTy

      case EFillRight(e, ev, c, t) => 
        for {
          pr <- inferCell(rho, gma, e)
          (cv, frm) = pr
          ed = frm.dim
          ee = eval(e, rho)
          a <- inferRightExt(rho, gma, ev)
          addr <- parseAddress(ed)(a)
          cell = frm >> Dot(ee, S(ed))
          cCell <- fromShape(frm.sourceAt(ed)(addr :: Nil))
          cTy <- cellType(ed)(cv, cCell)
          _ <- check(rho, gma, c, cTy)
          cVal = eval(c, rho)
          tNst <- fromShape(frm.head.replaceAt(addr :: Nil, cVal))
          _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))
          tVal = eval(t, rho)
          rext <- fromShape(cell.rightExtend(addr)(cVal, LiftRight(ee, eval(ev, rho), cVal, tVal), tVal))
        } yield Cell(cv, rext)

      //
      //  Property Inferences
      //

      case EDropIsLeft(c, e) => 
        for {
          _ <- checkI(rho, gma, EDrop(c, e))
          ef = eval(EDrop(c, e), rho)
        } yield IsLeftExt(ef)

      case EFillIsLeft(c, d, pd) => 
        for {
          _ <- checkI(rho, gma, EFill(c, d, pd))     // Infer that the fill is well-formed
          ef = eval(EFill(c, d, pd), rho)            // Evaluate it ...
        } yield IsLeftExt(ef)                        // and we know the type!

      case EShellIsLeft(e, ev, s, t) => 
        for {
          pr <- inferCell(rho, gma, e)
          (cv, frm) = pr
          ee = eval(e, rho)
          ed = frm.dim
          _ <- check(rho, gma, ev, IsLeftExt(ee))
          eTgt = frm.head.baseValue
          eSrc <- sourceTree(frm)
          srcTr <- parseTree(ed)(s)
          oTr <- fromShape(
            Tree.matchTraverse[Val, Expr, Option[Val], Nat](eSrc, srcTr)({
              case (u, EEmpty) => succeed(Some(u))
              case (u, v) => 
                for { 
                  _ <- toShape(check(rho, gma, v, IsLeftExt(u))) 
                } yield None
            })
          )
          ty <- (
            (oTr.nodes.filter(_.isDefined), t) match {
              case (Nil, EEmpty) => pure(IsLeftExt(eTgt))
              case (Some(u) :: Nil, tev) => 
                for {
                  _ <- check(rho, gma, tev, IsLeftExt(eTgt))
                } yield IsLeftExt(u)
              case _ => fail("Malformed shell evidence")
            }
          )
        } yield ty

      case EFillLeftIsLeft(e, ev, c, t) => 
        for {
          pr <- inferCell(rho, gma, e)
          (cv, frm) = pr
          ee = eval(e, rho)
          ed = frm.dim
          _ <- check(rho, gma, ev, IsLeftExt(ee))
          cell = frm >> Dot(ee, S(ed))
          cCell <- fromShape(cell.target)
          cTy <- cellType(ed)(cv, cCell)
          _ <- check(rho, gma, c, cTy)
          cVal = eval(c, rho)
          tNst <- fromShape(frm.head.replaceAt(Nil, cVal))
          _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))
          tVal = eval(t, rho)
        } yield IsLeftExt(FillLeft(ee, eval(ev, rho), cVal, tVal))

      case EFillRightIsLeft(e, ev, c, t) => 
        for {
          pr <- inferCell(rho, gma, e)
          (cv, frm) = pr
          ed = frm.dim
          ee = eval(e, rho)
          a <- inferRightExt(rho, gma, ev)
          addr <- parseAddress(ed)(a)
          cell = frm >> Dot(ee, S(ed))
          cCell <- fromShape(frm.sourceAt(ed)(addr :: Nil))
          cTy <- cellType(ed)(cv, cCell)
          _ <- check(rho, gma, c, cTy)
          cVal = eval(c, rho)
          tNst <- fromShape(frm.head.replaceAt(addr :: Nil, cVal))
          _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))
          tVal = eval(t, rho)
        } yield IsLeftExt(FillRight(ee, eval(ev, rho), cVal, tVal))

      case EFillLeftIsRight(e, ev, c, t, l, f, fev) => 
        for {
          pr <- inferCell(rho, gma, e)                           
          (cv, frm) = pr                                         
          ed = frm.dim                                           
          ee = eval(e, rho)                                      
          _ <- check(rho, gma, ev, IsLeftExt(ee))                
          cell = frm >> Dot(ee, S(ed))                           
          cCell <- fromShape(cell.target)                        
          cTy <- cellType(ed)(cv, cCell)                         
          _ <- check(rho, gma, c, cTy)                           
          cVal = eval(c, rho)                                    
          tNst <- fromShape(frm.head.replaceAt(Nil, cVal))       
          _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))  
          tVal = eval(t, rho)                                    
          lext <- fromShape(cell.leftExtend(cVal, Empty, tVal))  
          lCell <- fromShape(lext.sourceAt(Nil :: Nil))          
          lTy <- cellType(S(ed))(cv, lCell)                      
          _ <- check(rho, gma, l, lTy)
          lVal = eval(l, rho)
          lNst <- fromShape(lext.head.replaceAt(Nil :: Nil, lVal))
          _ <- check(rho, gma, f, Cell(cv, lext.withHead(lNst)))
          fVal = eval(f, rho)
          _ <- check(rho, gma, fev, IsLeftExt(fVal))
        } yield IsRightExt(fVal, ANil)

      case EFillRightIsRight(e, ev, c, t, l, f, fev) => 
        for {
          pr <- inferCell(rho, gma, e)
          (cv, frm) = pr
          ed = frm.dim
          ee = eval(e, rho)
          a <- inferRightExt(rho, gma, ev)
          addr <- parseAddress(ed)(a)
          cell = frm >> Dot(ee, S(ed))
          cCell <- fromShape(frm.sourceAt(ed)(addr :: Nil))
          cTy <- cellType(ed)(cv, cCell)
          _ <- check(rho, gma, c, cTy)
          cVal = eval(c, rho)
          tNst <- fromShape(frm.head.replaceAt(addr :: Nil, cVal))
          _ <- check(rho, gma, t, Cell(cv, frm.withHead(tNst)))
          tVal = eval(t, rho)
          rext <- fromShape(cell.rightExtend(addr)(cVal, Empty, tVal))
          lCell <- fromShape(rext.sourceAt((addr :: Nil) :: Nil))
          lTy <- cellType(S(ed))(cv, lCell)
          _ <- check(rho, gma, l, lTy)
          lVal = eval(l, rho)
          lNst <- fromShape(rext.head.replaceAt((addr :: Nil) :: Nil, lVal))
          _ <- check(rho, gma, f, Cell(cv, rext.withHead(lNst)))
          fVal = eval(f, rho)
          _ <- check(rho, gma, fev, IsLeftExt(fVal))
        } yield IsRightExt(fVal, rbAddr(S(ed))(addr :: Nil))

      // Oh crap ....
      case e => fail("checkI: " ++ e.toString)

    }

  }

}
