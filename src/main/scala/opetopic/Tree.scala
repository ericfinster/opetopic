/**
  * Tree.scala - Higher Dimensional Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Id._
import scalaz.Monad
import scalaz.Applicative
import scalaz.syntax.monad._

import TypeDefs._

sealed abstract class Tree[+A, N <: Nat] { def dim : N }
case class Pt[+A](a : A) extends Tree[A, _0] { def dim = Z }
case class Leaf[N <: Nat](d : S[N]) extends Tree[Nothing, S[N]] { def dim = d ; override def toString = "Leaf" }
case class Node[+A, N <: Nat](a : A, shell : Tree[Tree[A, S[N]], N]) extends Tree[A, S[N]] { def dim = S(shell.dim) }

trait TreeFunctions { tfns => 

  //============================================================================================
  // TRAVERSE
  //

  def traverse[T[_], A, B, N <: Nat](tr : Tree[A, N])(f : A => T[B])(implicit apT : Applicative[T]) : T[Tree[B, N]] = {

    import apT.{pure, ap, ap2}

    tr match {
      case Pt(a) => ap(f(a))(pure(Pt(_)))
      case Leaf(d) => pure(Leaf(d))
      case Node(a, sh) => ap2(f(a), traverse(sh)(traverse(_)(f)))(pure(Node(_, _)))
    }

  }

  //============================================================================================
  // TRAVERSE WITH ADDRESS
  //

  def traverseWithAddress[T[_], A, B, N <: Nat](
    tr : Tree[A, N], base : Address[N]
  )(f : (A, Address[N]) => T[B])(implicit apT : Applicative[T]) : T[Tree[B, N]] = {

    import apT.{pure, ap, ap2}

    def traverseShell[P <: Nat](
      a : A, sh : Tree[Tree[A, S[P]], P], 
      addr : Address[S[P]], 
      f : (A, Address[S[P]]) => T[B]
    ) : T[Tree[Tree[B, S[P]], P]] = {
      traverseWithAddress(sh)({
        case (br, dir) => traverseWithAddress(br, dir :: addr : Address[S[P]])(f)
      })
    }

    tr match {
      case Pt(a) => ap(f(a, base))(pure(Pt(_)))
      case Leaf(d) => pure(Leaf(d))
      case Node(a, sh) => ap2(f(a, base), traverseShell(a, sh, base, f))(pure(Node(_, _)))
    }

  }

  def traverseWithAddress[T[_], A, B, N <: Nat](tr : Tree[A, N])(f : (A, Address[N]) => T[B])(implicit apT : Applicative[T]) : T[Tree[B, N]] = 
    traverseWithAddress(tr, Zipper.rootAddr(tr.dim))(f)

  //============================================================================================
  // TRAVERSE WITH LOCAL DATA
  //

  def traverseWithLocalData[T[_], A, B, C, N <: Nat](
    tr : Tree[A, N], base : Address[N]
  )(f : (A, Address[N], Derivative[B, N]) => T[C])(implicit apT : Applicative[T]) : T[Tree[C, N]] = {

    import apT.{pure, ap, ap2, tuple2}

    def traverseShell[P <: Nat](
      a : A, sh : Tree[Tree[A, S[P]], P], 
      addr : Address[S[P]], 
      f : (A, Address[S[P]], Derivative[B, S[P]]) => T[C]
    ) : T[Tree[Tree[C, S[P]], P]] = {

      val p : P = sh.dim

      traverseWithAddress(sh)({
        case (br, dir) => traverseWithLocalData(br, dir :: addr : Address[S[P]])(f)
      })
    }

    tr match {
      case Pt(a) => ap(f(a, base, () : Derivative[B, _0]))(pure(Pt(_)))
      case Leaf(d) => pure(Leaf(d))
      case Node(a, sh) => {
        ap2(f(a, base, (const(sh, Leaf(S(sh.dim))), Nil) : Derivative[B, S[Nat]]), traverseShell(a, sh, base, f))(pure(Node(_, _)))
      }
    }
  }

  def traverseWithLocalData[T[_], A, B, C, N <: Nat](tr : Tree[A, N])(f : (A, Address[N], Derivative[B, N]) => T[C])(implicit apT : Applicative[T]) : T[Tree[C, N]] =
    traverseWithLocalData(tr, Zipper.rootAddr(tr.dim))(f)

  //============================================================================================
  // MATCH TRAVERSE
  //

  def matchTraverse[M[+_], A, B, C, N <: Nat](
    trA : Tree[A, N], trB : Tree[B, N]
  )(f : (A, B) => M[C])(implicit sm : ShapeMonad[M]) : M[Tree[C, N]] = {

    import sm.failWith

    val apM = Applicative[M]
    import apM.{pure, ap, ap2}

    (trA, trB) match {
      case (Pt(a), Pt(b)) => ap(f(a, b))(pure(Pt(_)))
      case (Leaf(d), Leaf(_)) => pure(Leaf(d))
      case (Node(a, shA), Node(b, shB)) => {

        val matchedShell = 
          matchTraverse(shA, shB)({
            case (brA, brB) => matchTraverse(brA, brB)(f)
          })

        ap2(f(a, b), matchedShell)(pure(Node(_, _)))

      }
      case _ => failWith(new ShapeMatchError)
    }

  }

  //============================================================================================
  // MATCH WITH DERIVATIVE
  //

  def matchWithDerivative[M[+_], A, B, C, D, N <: Nat](
    trA : Tree[A, N], trB : Tree[B, N]
  )(f : (A, B, Derivative[C, N]) => M[D])(implicit sm : ShapeMonad[M]) : M[Tree[D, N]] = {

    import sm.failWith

    val apM = Applicative[M]
    import apM.{pure, ap, ap2}

    (trA, trB) match {
      case (Pt(a), Pt(b)) => ap(f(a, b, ()))(pure(Pt(_)))
      case (Leaf(d), Leaf(_)) => pure(Leaf(d))
      case (Node(a, shA), Node(b, shB)) => {

        val matchedShell = 
          matchTraverse(shA, shB)({
            case (brA, brB) => matchWithDerivative(brA, brB)(f)
          })

        val deriv : Derivative[C, S[Nat]] = 
          (const[Tree[B, S[Nat]], Tree[C, S[Nat]], Nat](shB, Leaf(S(shB.dim))), Nil)

        ap2(f(a, b, deriv), matchedShell)(pure(Node(_, _)))

      }
      case _ => failWith(new ShapeMatchError)
    }
  }

  //============================================================================================
  // GRAFTING
  //

  abstract class GraftRecursor[M[+_], A, B, P <: Nat](implicit sm : ShapeMonad[M]) {

    def caseLeaf(addr: Address[S[P]]) : M[B]
    def caseNode(a: A, sh: Tree[B, S[P]]) : M[B]

    def unzipAndJoin(zt: Tree[(Tree[B, S[P]], Tree[Address[S[P]], P]), P]) : M[(Tree[Tree[B, S[P]], P], Tree[Address[S[P]], P])] = {
      val (bSh, adJnSh) = unzip(zt)
      for {
        adTr <- join(adJnSh)
      } yield (bSh, adTr)
    }

    def unzipJoinAndAppend(zt : Tree[(Tree[B, S[P]], Tree[Address[S[P]], P]), P], mb : M[B]) : M[(Tree[B, S[P]], Tree[Address[S[P]], P])] = 
      for {
        pr <- unzipAndJoin(zt)
        (bSh, adTr) = pr
        b <- mb
      } yield (Node(b, bSh), adTr)

    def horizontalPass(base: Address[S[P]], hbr: Tree[Tree[A, S[S[P]]], S[P]], deriv : Derivative[Address[S[P]], P]) : M[(Tree[B, S[P]], Tree[Address[S[P]], P])] = 
      hbr match {
        case Leaf(sp) => sm.point((Leaf(sp), Zipper.plug(sp.pred)(deriv, base)))
        case Node(Leaf(ssp), hsh) => 
          for {
            hres <- traverseWithLocalData(hsh)({
              case (hbr, d, deriv0) => horizontalPass(d :: base, hbr, deriv0)
            })
            res <- unzipJoinAndAppend(hres, caseLeaf(base))
          } yield res
        case Node(Node(a, vsh), hsh) => 
          for {
            pr <- horizontalPass(base, vsh, deriv)
            (bTr, adTr0) = pr
            mres <- matchWithDerivative(adTr0, hsh)(horizontalPass(_,_,_))
            res <- unzipJoinAndAppend(mres, caseNode(a, bTr))
          } yield res
      }

    def initHorizontal(a: A, hsh: Tree[Tree[Tree[A, S[S[P]]], S[P]], P])(m: M[(B, Tree[Address[S[P]], P])]) : M[(B, Tree[Address[S[P]], P])] = 
      for {
        pr0 <- m
        (b0, adTr0) = pr0
        res <- matchWithDerivative(adTr0, hsh)(horizontalPass(_, _, _))
        pr1 <- unzipAndJoin(res)
        (bSh, adTr) = pr1
        b <- caseNode(a, Node(b0, bSh))
      } yield (b, adTr)

    def initVertical(a0: A, v: Tree[A, S[S[P]]], hsh0: Tree[Tree[Tree[A, S[S[P]]], S[P]], P]) : M[(B, Tree[Address[S[P]], P])] = 
      v match {
        case Leaf(ssp) => initHorizontal(a0, hsh0)(
          for { 
            b <- caseLeaf(Nil)
          } yield (b, mapWithAddress(hsh0)({ case (_, d) => d :: Nil : Address[S[P]] }))
        )
        case Node(a1, Leaf(sp)) => initHorizontal(a0, hsh0)(
          for { 
            b <- caseNode(a0, Leaf(sp)) 
          } yield (b, const(hsh0, Nil))
        )
        case Node(a1, Node(v, hsh1)) => 
          initHorizontal(a0, hsh0)(initVertical(a1, v, hsh1))
      }

  }

  def graftRec[M[+_], A, B, N <: Nat](tr : Tree[A, S[N]])(leafRec : Address[N] => M[B])(nodeRec : (A, Tree[B, N]) => M[B])(implicit sm : ShapeMonad[M]) : M[B] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (Tree[A, S[N]], Address[N] => M[B], (A, Tree[B, N]) => M[B]) => M[B]

      def caseZero : Out[_0] = {
        case (Leaf(_), lfR, ndR) => lfR(())
        case (Node(hd, Pt(tl)), lfR, ndR) => 
          for {
            b0 <- caseZero(tl, lfR, ndR)
            b <- ndR(hd, Pt(b0))
          } yield b
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (Leaf(_), lfR, ndR) => lfR(Nil)
        case (Node(a, Leaf(d)), lfR, ndR) => ndR(a, Leaf(d))
        case (Node(a, Node(v, hsh)), lfR, ndR) => {

          val recursor = new GraftRecursor[M, A, B, P] { 

            def caseLeaf(addr: Address[S[P]]) : M[B] = lfR(addr)
            def caseNode(a: A, sh: Tree[B, S[P]]) : M[B] = ndR(a, sh)

          }

          for {
            pr <- recursor.initVertical(a, v, hsh)
          } yield pr._1
        }
      }

    })(tr.dim.pred)(tr, leafRec, nodeRec)

  def graft[M[+_], A, N <: Nat](tr : Tree[A, S[N]])(brs : Tree[Tree[A, S[N]], N])(implicit sm : ShapeMonad[M]) : M[Tree[A, S[N]]] = 
    graftRec(tr)(addr => valueAt(brs, addr))({ case (a, sh) => sm.pure(Node(a, sh)) })

  //============================================================================================
  // JOIN
  //

  def join[M[+_], A, N <: Nat](tr : Tree[Tree[A, N], N])(implicit sm : ShapeMonad[M]) : M[Tree[A, N]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = Tree[Tree[A, N], N] => M[Tree[A, N]]

      def caseZero : Out[_0] = {
        case Pt(t) => sm.pure(t)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case Leaf(d) => sm.pure(Leaf(d))
        case Node(t, tsh) => 
          for {
            gsh <- traverse(tsh)(join(_))
            res <- graft(t)(gsh)
          } yield res
      }

    })(tr.dim)(tr)


  //============================================================================================
  // SHELL EXTENTS
  //


  def shellExtents[M[+_], A, N <: Nat](sh: Tree[Tree[A, S[N]], N], base: Address[S[N]])(implicit sm: ShapeMonad[M]) : M[Tree[Address[S[N]], N]] = 
    for {
      jnSh <- traverseWithLocalData(sh)({ 
        case (Leaf(d), dir, deriv) => 
          sm.pure(Zipper.plug(d.pred)(deriv, dir :: base))
        case (Node(_, sh0), dir, deriv) => shellExtents(sh0, dir :: base)
      })
      res <- join(jnSh)
    } yield res

  def shellExtents[M[+_], A, N <: Nat](sh: Tree[Tree[A, S[N]], N])(implicit sm: ShapeMonad[M]) : M[Tree[Address[S[N]], N]] = 
    shellExtents(sh, Nil)

  //============================================================================================
  // SPLIT WITH
  //

  def splitWith[A, B, C, N <: Nat](tr : Tree[A, N])(f : A => (B, C)) : (Tree[B, N], Tree[C, N]) = 
    tr match {
      case Pt(a) => {
        val (b, c) = f(a)
        (Pt(b), Pt(c))
      }
      case Leaf(d) => (Leaf(d), Leaf(d))
      case Node(a, sh) => {
        val (b, c) = f(a)
        val (shB, shC) = unzip(map(sh)(splitWith(_)(f)))
        (Node(b, shB), Node(c, shC))
      }
    }

  //============================================================================================
  // UNZIP
  //

  def unzip[A, B, N <: Nat](tr : Tree[(A, B), N]) : (Tree[A, N], Tree[B, N]) =
    splitWith(tr)({ case (a, b) => (a, b) })

  //============================================================================================
  // FOREACH
  //

  def foreach[A, N <: Nat](tr : Tree[A, N])(op : A => Unit) : Unit = 
    tr match {
      case Pt(a) => op(a)
      case Leaf(d) => ()
      case Node(a, sh) => {
        foreach(sh)(foreach(_)(op))
        op(a)
      }
    }

  //============================================================================================
  // ROOT VALUE
  //

  def rootValue[M[+_], A, N <: Nat](tr : Tree[A, N])(implicit sm : ShapeMonad[M]) : M[A] =
    tr match {
      case Pt(a) => sm.pure(a)
      case Leaf(_) => sm.failWith(new ShapeRootEmptyError)
      case Node(a, _) => sm.pure(a)
    }

  //============================================================================================
  // MAP IMPLEMENTATIONS
  //

  def map[A, B, N <: Nat](tr : Tree[A, N])(f : A => B) : Tree[B, N] = 
    traverse[Id, A, B, N](tr)(f)

  def mapWithAddress[A, B, N <: Nat](tr : Tree[A, N])(f : (A, Address[N]) => B) : Tree[B, N] =
    traverseWithAddress[Id, A, B, N](tr)(f)

  def const[A, B, N <: Nat](tr : Tree[A, N], b : B) : Tree[B, N] = 
    map(tr)(_ => b)

  //============================================================================================
  // SEEK TO
  //

  def seekTo[M[+_], A, N <: Nat](tr : Tree[A, N], addr : Address[N])(implicit sm : ShapeMonad[M]) : M[Zipper[A, N]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (Tree[A, N], Address[N]) => M[Zipper[A, N]]

      def caseZero : Out[_0] = {
        case (tr, addr) => sm.pure((tr, ()))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (tr, addr) => Zipper.seek(S(p))((tr, Nil), addr)
      }

    })(tr.dim)(tr, addr)


  def valueAt[M[+_], A, N <: Nat](tr : Tree[A, N], addr : Address[N])(implicit sm : ShapeMonad[M]) : M[A] = 
    for {
      zp <- seekTo(tr, addr)
      v <- rootValue(zp._1)
    } yield v

  //============================================================================================
  // TAKE WHILE
  //

  def takeWhile[A, N <: Nat](tr : Tree[A, S[N]])(p : A => Boolean) : Tree[A, S[N]] = 
    tr match {
      case Leaf(d) => Leaf(d)
      case Node(a, sh) => 
        if (p(a)) {
          Node(a, map(sh)(takeWhile(_)(p)))
        } else {
          Leaf(S(sh.dim))
        }
    }

  //============================================================================================
  // EXCISION
  //

  def exciseWithMask[M[+_], A, B, N <: Nat](tr: Tree[A, S[N]], deriv: Derivative[Tree[A, S[N]], N], msk: Tree[B, S[N]])(
    implicit sm: ShapeMonad[M]
  ) : M[(Tree[A, S[N]], Tree[Tree[A, S[N]], N])] = 
    (tr, msk) match {
      case (tr, Leaf(d)) => sm.pure(Leaf(d), Zipper.plug(d.pred)(deriv, tr))
      case (Leaf(_), Node(_, _)) => sm.failWith(new ShapeLookupError)
      case (Node(a, sh), Node(_, mskSh)) =>
        for {
          ztr <- matchWithDerivative(sh, mskSh)({ case (t, m0, d0) => exciseWithMask(t, d0, m0) })
          (nsh, crpJn) = unzip(ztr)
          crp <- join(crpJn)
        } yield (Node(a, nsh), crp)
    }

  def exciseWithMask[M[+_], A, B, N <: Nat](tr: Tree[A, S[N]], msk: Tree[B, S[N]])(
    implicit sm: ShapeMonad[M]
  ) : M[(Tree[A, S[N]], Tree[Tree[A, S[N]], N])] = 
    exciseWithMask(tr, Zipper.globDerivative(tr.dim.pred), msk)

}

object Tree extends TreeFunctions 


