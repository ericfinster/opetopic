/**
  * Tree.scala - Higher Dimensional Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Id._
import scalaz.Traverse 
import scalaz.Applicative
import scalaz.std.option._

import Nat._
import Zippers._

sealed abstract class Tree[N <: Nat, +A] { def dim : N }
case class Pt[+A](a : A) extends Tree[_0, A] { def dim = Z }
case class Leaf[N <: Nat](d : S[N]) extends Tree[S[N], Nothing] { def dim = d ; override def toString = "Leaf" }
case class Node[N <: Nat, +A](a : A, shell : Tree[N, Tree[S[N], A]]) extends Tree[S[N], A] { def dim = S(shell.dim) }

trait TreeFunctions { tfns => 

  //============================================================================================
  // CASE SPLITTING
  //

  trait TreeCaseSplit[A] {

    type Out[N <: Nat]

    def casePt(a : A) : Out[_0]
    def caseLeaf[P <: Nat](d : S[P]) : Out[S[P]]
    def caseNode[P <: Nat](a : A, sh : Tree[P, Tree[S[P], A]]) : Out[S[P]]

    def apply[N <: Nat](tr : Tree[N, A]) : Out[N] = 
      tr match {
        case Pt(a) => casePt(a)
        case Leaf(d) => caseLeaf(d)
        case Node(a, sh) => caseNode(a, sh)
      }

  }

  //============================================================================================
  // TRAVERSE
  //

  def traverse[N <: Nat, T[_], A, B](tr : Tree[N, A])(f : A => T[B])(implicit apT : Applicative[T]) : T[Tree[N, B]] = {

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

  def traverseWithAddress[N <: Nat, T[_], A, B](
    tr : Tree[N, A], base : Address[N]
  )(f : (A, Address[N]) => T[B])(implicit apT : Applicative[T]) : T[Tree[N, B]] = {

    import apT.{pure, ap, ap2}

    def traverseShell[P <: Nat](
      a : A, sh : Tree[P, Tree[S[P], A]], 
      addr : Address[S[P]], 
      f : (A, Address[S[P]]) => T[B]
    ) : T[Tree[P, Tree[S[P], B]]] = {
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

  def traverseWithAddress[N <: Nat, T[_], A, B](tr : Tree[N, A])(f : (A, Address[N]) => T[B])(implicit apT : Applicative[T]) : T[Tree[N, B]] = 
    traverseWithAddress(tr, rootAddr(tr.dim))(f)

  //============================================================================================
  // MAP IMPLEMENTATIONS
  //

  def map[N <: Nat, A, B](tr : Tree[N, A])(f : A => B) : Tree[N, B] = 
    traverse[N, Id, A, B](tr)(f)

  def mapWithAddress[N <: Nat, A, B](tr : Tree[N, A])(f : (A, Address[N]) => B) : Tree[N, B] =
    traverseWithAddress[N, Id, A, B](tr)(f)

  def const[N <: Nat, A, B](tr : Tree[N, A], b : B) : Tree[N, B] = 
    map(tr)(_ => b)

  //============================================================================================
  // FOREACH
  //

  def foreach[N <: Nat, A](tr : Tree[N, A])(op : A => Unit) : Unit = 
    tr match {
      case Pt(a) => op(a)
      case Leaf(d) => ()
      case Node(a, sh) => {
        foreach(sh)(foreach(_)(op))
        op(a)
      }
    }

  //============================================================================================
  // TREE ELIMINATORS
  //

  trait TreeElim[A] { thisElim =>

    type Out[N <: Nat]

    def casePt(a : A) : Out[_0]
    def caseLeaf[P <: Nat](d : S[P]) : Out[S[P]]
    def caseNode[P <: Nat](a : A, sh : Tree[P, Out[S[P]]]) : Out[S[P]]

    def apply[N <: Nat](tr : Tree[N, A]) : Out[N] = 
      eliminator(tr)

    object eliminator extends TreeCaseSplit[A] {

      type Out[N <: Nat] = thisElim.Out[N]

      def casePt(a : A) : Out[_0] =
        thisElim.casePt(a)

      def caseLeaf[P <: Nat](d : S[P]) : Out[S[P]] =
        thisElim.caseLeaf(d)

      def caseNode[P <: Nat](a : A, sh : Tree[P, Tree[S[P], A]]) : Out[S[P]] =
        thisElim.caseNode(a, map(sh)(eliminator(_)))

    }

  }

  trait TreeAddrElim[A] { thisElim =>

    type Out[N <: Nat]

    def casePt(a : A) : Out[_0]
    def caseLeaf[P <: Nat](addr : Address[S[P]], d : S[P]) : Out[S[P]]
    def caseNode[P <: Nat](addr : Address[S[P]], a : A, ih : Tree[P, Out[S[P]]]) : Out[S[P]]

    def apply[N <: Nat](tr : Tree[N, A], addr : Address[N]) : Out[N] = 
      eliminator(tr)(addr)

    def apply[N <: Nat](tr : Tree[N, A]) : Out[N] = 
      this(tr, rootAddr(tr.dim))

    object eliminator extends TreeCaseSplit[A] {

      type Out[N <: Nat] = Address[N] => thisElim.Out[N]

      def casePt(a : A) : Out[_0] =
        _ => thisElim.casePt(a)

      def caseLeaf[P <: Nat](d : S[P]) : Out[S[P]] =
        addr => thisElim.caseLeaf(addr, d)

      def caseNode[P <: Nat](a : A, sh : Tree[P, Tree[S[P], A]]) : Out[S[P]] =
        addr => thisElim.caseNode(addr, a, mapWithAddress(sh)(
          (br , dir) => eliminator(br)(dir :: addr : Address[S[P]])
        ))

    }

  }

  //============================================================================================
  // MATCH TREE
  //

  def matchTree[N <: Nat, A, B](trA : Tree[N, A], trB : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
    (trA, trB) match {
      case (Pt(a), Pt(b)) => Some(Pt((a, b)))
      case (Leaf(d), Leaf(_)) => Some(Leaf(d))
      case (Node(a, ash), Node(b, bsh)) =>
        for {
          zsh <- matchTree(ash, bsh)
          psh <- traverse(zsh)({
            case (at, bt) => matchTree(at, bt)
          })
        } yield Node((a, b), psh)

      case _ => None
    }

  //============================================================================================
  // ZIP WITH DERIVATIVE
  //

  def zipWithDerivative[N <: Nat, A, B](tr : Tree[N, A]) : Tree[N, (Derivative[N, B], A)] = 
    (new TreeCaseSplit[A] {

      type Out[N <: Nat] = Tree[N, (Derivative[N, B], A)]

      def casePt(a : A) : Out[_0] = 
        Pt((), a)

      def caseLeaf[P <: Nat](d : S[P]) : Out[S[P]] =
        Leaf(d)

      def caseNode[P <: Nat](a : A, sh : Tree[P, Tree[S[P], A]]) : Out[S[P]] = 
        Node(((sh constWith Leaf(S(sh.dim)), Nil), a), map(sh)(zipWithDerivative(_)))

    })(tr)

  //============================================================================================
  // ROOT OPTION
  //

  def rootOption[N <: Nat, A](tr : Tree[N, A]) : Option[A] =
    tr match {
      case Pt(a) => Some(a)
      case Leaf(_) => None
      case Node(a, _) => Some(a)
    }

  //============================================================================================
  // UNZIP
  //

  def unzip[N <: Nat, A, B](tr : Tree[N, (A, B)]) : (Tree[N, A], Tree[N, B]) = 
    (new TreeCaseSplit[(A, B)] {

      type Out[N <: Nat] = (Tree[N, A], Tree[N, B])

      def casePt(pr : (A, B)) : Out[_0] =
        (Pt(pr._1), Pt(pr._2))

      def caseLeaf[P <: Nat](d : S[P]) : Out[S[P]] = 
        (Leaf(d), Leaf(d))

      def caseNode[P <: Nat](pr : (A, B), sh : Tree[P, Tree[S[P], (A, B)]]) : Out[S[P]] = {
        val (aSh, bSh) = unzip(map(sh)(unzip(_)))
        (Node(pr._1, aSh), Node(pr._2, bSh))
      }

    })(tr)

  //============================================================================================  
  // LOCAL DATA
  //

  def localData[N <: Nat, A, B](tr : Tree[N, A]) : Tree[N, (Address[N], Derivative[N, B], A)] = 
    (new TreeAddrElim[A] {

      type Out[N <: Nat] = Tree[N, (Address[N], Derivative[N, B], A)]

      def casePt(a : A) : Out[_0] = 
        Pt((), (), a)

      def caseLeaf[P <: Nat](addr : Address[S[P]], d : S[P]) : Out[S[P]] = 
        Leaf(d)

      def caseNode[P <: Nat](addr : Address[S[P]], a : A, ih : Tree[P, Out[S[P]]]) : Out[S[P]] = 
        Node((addr, (ih constWith Leaf(S(ih.dim)), Nil), a), ih)

    })(tr)

  def extentsData[N <: Nat, A](tr : Tree[N, A]) : Tree[N, (Address[N], Derivative[N, Address[S[N]]], A)] =
    localData[N, A, Address[S[N]]](tr)

  def shellExtentsFrom[N <: Nat, A](shell : Tree[N, Tree[S[N], A]], addr : Address[S[N]]) : Option[Tree[N, Address[S[N]]]] =
    for {
      rec <- extentsData(shell).traverse({
        case (dir, deriv, Leaf(_)) => Some(plug(shell.dim)(deriv, dir :: addr)) 
        case (dir, deriv, Node(_, sh)) => shellExtentsFrom(sh, dir :: addr)
      })
      res <- join(rec)
    } yield res

  def shellExtents[N <: Nat, A](shell : Tree[N, Tree[S[N], A]]) : Option[Tree[N, Address[S[N]]]] = 
    shellExtentsFrom(shell, rootAddr(S(shell.dim)))

  //============================================================================================
  // GRAFT ELIMINATION
  //

  trait TreeGraftElim[P <: Nat, A, B] {

    def caseLeaf(addr : Address[P]) : Option[B]
    def caseNode(a : A, sh : Tree[P, B]) : Option[B]

    def apply(tr : Tree[S[P], A]) : Option[B] = 
      graftElim(tr)(this)

  }

  // All this seems to work as expected.  Just needs to be cleaned up and simplified since it
  // is really in a performance critical position.  Ideas include a custom traverse which runs
  // a computation on two trees as they are being zipped and a trait which captures the node
  // and leaf recursion functions to simplify the API.

  def graftRecHoriz[N <: Nat, A, B](
    elim : TreeGraftElim[S[N], A, B],
    addr : Address[S[N]],
    deriv : Derivative[N, Address[S[N]]],
    tr : Tree[S[N], Tree[S[S[N]], A]]
  ) : Option[(Tree[S[N], B], Tree[N, Address[S[N]]])] = 
    tr match {
      case Leaf(d) => Some(Leaf(d), plug(tr.dim.pred)(deriv, addr))
      case Node(Leaf(d), hsh) => {

        val contData = map(localData[N, Tree[S[N], Tree[S[S[N]], A]], Address[S[N]]](hsh))({
          case (dir, deriv, vsh) => (dir :: addr, (deriv, vsh))
        })

        for { 
          res <- graftElimCont(elim, contData)
          (bSh , adTr) = res
          b <- elim.caseLeaf(addr)
        } yield (Node(b, bSh), adTr)
      }
      case Node(Node(a, vsh), hsh) => 
        for {
          pr <- graftRecHoriz(elim, addr, deriv, vsh)
          (bTr, adTr0) = pr
          zt <- matchTree(adTr0,  hsh.zipWithDerivative[Address[S[N]]]) 
          res <- graftElimCont(elim, zt)
          (bSh, adTr) = res
          b <- elim.caseNode(a, bTr)
        } yield (Node(b, bSh), adTr)
    }

  def graftElimCont[N <: Nat, A, B](
    elim : TreeGraftElim[S[N], A, B],
    contData : Tree[N, (Address[S[N]], (Derivative[N, Address[S[N]]], Tree[S[N], Tree[S[S[N]], A]]))]
  ) : Option[(Tree[N, Tree[S[N], B]], Tree[N, Address[S[N]]])] = 
    for {
      trRes <- contData.traverse({ case (addr, (deriv, nsh)) => graftRecHoriz(elim, addr, deriv, nsh) })
      (bSh, adrJnSh) = unzip(trRes)
      adSh <- join(adrJnSh)
    } yield (bSh, adSh)

  def graftElimChain[N <: Nat, A, B](
    elim : TreeGraftElim[S[N], A, B],
    a : A,
    hsh : Tree[N, Tree[S[N], Tree[S[S[N]], A]]],
    prevOpt : Option[(B, Tree[N, Address[S[N]]])]
  ) : Option[(B, Tree[N, Address[S[N]]])] = 
    for {
      pr0 <- prevOpt
      (b0, adTr0) = pr0
      zt <- matchTree(adTr0, hsh.zipWithDerivative[Address[S[N]]])
      pr <- graftElimCont(elim, zt)
      (bSh, adTr) = pr
      b <- elim.caseNode(a, Node(b0, bSh))
    } yield (b, adTr)

  def graftElimStart[N <: Nat, A, B](
    elim : TreeGraftElim[S[N], A, B], 
    tr : Tree[S[S[N]], A],
    a : A,
    hsh : Tree[N, Tree[S[N], Tree[S[S[N]], A]]]
  ) : Option[(B, Tree[N, Address[S[N]]])] = 
    tr match {
      case Leaf(dim) => 
        graftElimChain(elim, a, hsh,
          for {
            b <- elim.caseLeaf(rootAddr(dim.pred))
          } yield (b, mapWithAddress(hsh)((_, dir) => dir :: rootAddr(dim.pred)))
        )
      case Node(a0, Leaf(dim)) => 
        graftElimChain(elim, a, hsh,
          for {
            b <- elim.caseNode(a0, Leaf(dim))
          } yield (b, mapWithAddress(hsh)((dir, _) => rootAddr(dim))) 
        )
      case Node(a0, Node(v, hsh0)) =>
        graftElimChain(elim, a, hsh, graftElimStart(elim, v, a0, hsh0))
    }

  def graftElim[N <: Nat, A, B](tr : Tree[S[N], A])(elim : TreeGraftElim[N, A, B]) : Option[B] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = (Tree[S[N], A], TreeGraftElim[N, A, B]) => Option[B]

      def caseZero : Out[_0] = {
        case (Leaf(d), elim) => elim.caseLeaf(rootAddr(__0))
        case (Node(hd, Pt(tl)), elim) => 
          for {
            b0 <- caseZero(tl, elim)
            b <- elim.caseNode(hd, Pt(b0))
          } yield b
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (Leaf(d), elim) => elim.caseLeaf(rootAddr(d.pred))
        case (Node(a, Leaf(d)), elim) => elim.caseNode(a, Leaf(d))
        case (Node(a, Node(v, hsh)), elim) =>
          for {
            res <- graftElimStart(elim, v, a, hsh)
          } yield res._1
      }

    })(tr.dim.pred)(tr, elim)

  //============================================================================================
  // GRAFT
  //

  def graft[N <: Nat, A](tr : Tree[S[N], A], brs : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] = 
    (new TreeGraftElim[N, A, Tree[S[N], A]] {

      def caseLeaf(addr : Address[N]) : Option[Tree[S[N], A]] = 
        brs valueAt addr

      def caseNode(a : A, sh : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] = 
        Some(Node(a, sh))

    })(tr)

  //============================================================================================
  // JOIN
  //

  def join[N <: Nat, A](tr : Tree[N, Tree[N, A]]) : Option[Tree[N, A]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = Tree[N, Tree[N, A]] => Option[Tree[N, A]]

      def caseZero : Tree[_0, Tree[_0, A]] => Option[Tree[_0, A]] = {
        case Pt(t) => Some(t)
      }
        
      def caseSucc[P <: Nat](p : P) : Tree[S[P], Tree[S[P], A]] => Option[Tree[S[P], A]] = {
        case Leaf(d) => Some(Leaf(d))
        case Node(t, tsh) => 
          for {
            gsh <- traverse(tsh)(join(_))
            str <- graft(t, gsh)
          } yield str
      }

    })(tr.dim)(tr)

  //============================================================================================
  // FLATTEN
  //

  def flatten[N <: Nat, A](ds : Address[S[N]], deriv : Derivative[N, Address[S[N]]], tr : Tree[S[N], A]) : Option[Tree[N, Address[S[N]]]] = 
    tr match {
      case Leaf(d) => Some(plug(d.pred)(deriv, ds))
      case Node(a, sh) => 
        for {
          jnSh <- traverse(localData[N, Tree[S[N], A], Address[S[N]]](sh))({
            case (dir, deriv, v) => flatten[N, A](dir :: ds, deriv, v)
          })
          res <- join(jnSh)
        } yield res

    }

  def flatten[N <: Nat, A](deriv : Derivative[N, Address[S[N]]], tr : Tree[S[N], A]) : Option[Tree[N, Address[S[N]]]] = 
    flatten(rootAddr(tr.dim), deriv, tr)

  def flatten[N <: Nat, A](tr : Tree[S[N], A]) : Option[Tree[N, Address[S[N]]]] = 
    flatten(globDerivative(tr.dim.pred), tr)

  //============================================================================================
  // TAKE WHILE
  //

  def takeWhile[N <: Nat, A](tr : Tree[S[N], A])(p : A => Boolean) : Tree[S[N], A] = 
    tr match {
      case Leaf(d) => Leaf(d)
      case Node(a, sh) => 
        if (p(a)) {
          Node(a, sh map (takeWhile(_)(p)))
        } else {
          Leaf(S(sh.dim))
        }
    }

  //============================================================================================
  // EXCISION
  //

  def exciseDeriv[N <: Nat, A, B](deriv : Derivative[N, Tree[S[N], A]], tr : Tree[S[N], A], msk : Tree[S[N], B]) : Option[(Tree[S[N], A], Tree[N, Tree[S[N], A]])] =
    (tr, msk) match {
      case (tr, Leaf(d)) => Some(Leaf(d), plug(d.pred)(deriv, tr))
      case (Leaf(_), Node(_, _)) => None
      case (Node(a, sh), Node(_, mskSh)) => 
        for {
          zpSh <- matchTree(sh.zipWithDerivative[Tree[S[N], A]], mskSh) 
          zsh <- traverse(zpSh)({
            case ((d, t), m) => exciseDeriv(d, t, m)
          })
          (nsh, crpJn) = unzip(zsh)
          crp <- join(crpJn)
        } yield (Node(a, nsh), crp)
    }

  def excise[N <: Nat, A, B](tr : Tree[S[N], A], msk : Tree[S[N], B]) : Option[(Tree[S[N], A], Tree[N, Tree[S[N], A]])] =
    exciseDeriv(globDerivative(tr.dim.pred), tr, msk)

  def replace[N <: Nat, A, B](tr : Tree[S[N], A], msk : Tree[S[N], B], rpl : Tree[S[N], A] => A) : Option[Tree[S[N], A]] = 
    for {
      exPr <- excise(tr, msk)
      (exTr, exSh) = exPr
    } yield Node(rpl(exTr), exSh)

}

trait TreeImplicits {

  implicit def treeIsTraverse[N <: Nat] : Traverse[({ type L[+A] = Tree[N, A] })#L] = 
    new Traverse[({ type L[+A] = Tree[N, A] })#L] {

      override def map[A, B](ta : Tree[N, A])(f : A => B) : Tree[N, B] = 
        Tree.map(ta)(f)

      def traverseImpl[G[_], A, B](ta : Tree[N, A])(f : A => G[B])(implicit isA : Applicative[G]) : G[Tree[N, B]] = 
        Tree.traverse(ta)(f)

    }

  import scalaz.syntax.FunctorOps
  import scalaz.syntax.functor._

  implicit def treeToFunctorOps[N <: Nat, A](tr : Tree[N, A]) : FunctorOps[({ type L[+X] = Tree[N, X] })#L, A] = 
    ToFunctorOps[({ type L[+X] = Tree[N, X] })#L, A](tr)

  import scalaz.syntax.TraverseOps
  import scalaz.syntax.traverse._

  implicit def treeToTraverseOps[N <: Nat, A](tr : Tree[N, A]) : TraverseOps[({ type L[+X] = Tree[N, X] })#L, A] = 
    ToTraverseOps[({ type L[+X] = Tree[N, X] })#L, A](tr)

  class TreeOps[N <: Nat, A](tr : Tree[N, A]) {

    val T = Traverse[({ type L[+A] = Tree[N, A] })#L]

    def foreach(op : A => Unit) : Unit = 
      Tree.foreach(tr)(op)

    def mapWithAddress[B](f : (A, Address[N]) => B) : Tree[N, B] = 
      Tree.mapWithAddress(tr)(f)

    def zipWithDerivative[B] : Tree[N, (Derivative[N, B], A)] = 
      Tree.zipWithDerivative[N, A, B](tr)

    def zipWithAddress : Tree[N, (A, Address[N])] = 
      Tree.mapWithAddress(tr)((_, _))

    def addressTree : Tree[N, Address[N]] =
      Tree.mapWithAddress(tr)({ case (_, addr) => addr })

    def matchWith[B](trB : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
      Tree.matchTree(tr, trB)

    def seekTo(addr : Address[N]) : Option[Zipper[N, A]] = 
      (new NatCaseSplit {

        type Out[N <: Nat] = (Address[N], Tree[N, A]) => Option[Zipper[N, A]]

        def caseZero : (Address[_0], Tree[_0, A]) => Option[Zipper[_0, A]] = 
          (addr, tr) => Some(tr, ())

        def caseSucc[P <: Nat](p : P) : (Address[S[P]], Tree[S[P], A]) => Option[Zipper[S[P], A]] = 
          (addr, tr) => Zippers.seek((tr, Nil : Context[S[P], A]), addr)

      })(tr.dim)(addr, tr)

    def rootOption : Option[A] = 
      Tree.rootOption(tr)

    def constWith[B](b : B) : Tree[N, B] = 
      Tree.map(tr)(_ => b)

    def nodes : List[A] = T.toList(tr)
    def nodeCount : Int = T.count(tr)
    def zipWithIndex : (Int, Tree[N, (A, Int)]) = 
      T.mapAccumL(tr, 0)((i : Int, a : A) => (i + 1, (a, i)))

    def valueAt(addr : Address[N]) : Option[A] = 
      for {
        zipper <- tr seekTo addr
        a <- zipper.focus.rootOption
      } yield a

  }

  implicit def treeToTreeOps[N <: Nat, A](tr : Tree[N, A]) : TreeOps[N, A] = 
    new TreeOps[N, A](tr)

}

object Tree extends TreeFunctions 
    with TreeImplicits



