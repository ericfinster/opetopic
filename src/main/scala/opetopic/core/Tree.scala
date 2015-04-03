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
import scalaz.Applicative
import scalaz.Monad

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
    traverseWithAddress(tr, rootAddr(tr.dim))(f)

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
    traverseWithLocalData(tr, rootAddr(tr.dim))(f)

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
  // ROOT OPTION
  //

  def rootOption[A, N <: Nat](tr : Tree[A, N]) : Option[A] =
    tr match {
      case Pt(a) => Some(a)
      case Leaf(_) => None
      case Node(a, _) => Some(a)
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
  // GRAFTING
  //

  abstract class GraftRecursor[M[+_], A, B, P <: Nat](implicit sm : ShapeMonad[M]) {

    import scalaz.syntax.monad._

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
        case Leaf(sp) => sm.point((Leaf(sp), ???))
        case Node(Leaf(ssp), hsh) => ???
        case Node(Node(a, vsh), hsh) => ???
      }

    // horizontalPass : Address (suc n) → Tree (Tree A (suc (suc n))) (suc n) → Derivative (Address (suc n)) n → M (Tree B (suc n) × Tree (Address (suc n)) n)
    // horizontalPass base leaf ∂ = η (leaf , ∂ ← base)
    // horizontalPass base (node leaf hsh) ∂ =
    //   traverseWithLocalData apM hsh (λ hbr d ∂₀ → horizontalPass (d ∷ base) hbr ∂₀)
    //   >>= (λ res → unzipJoinAndAppend res (λ-rec base))
    // horizontalPass base (node (node a vsh) hsh) ∂ =
    //   horizontalPass base vsh ∂
    //   >>= (λ { (bTr , adTr₀) → matchWithDerivative isE {C = Address (suc n)} horizontalPass adTr₀ hsh
    //   >>= (λ res → unzipJoinAndAppend res (ν-rec a bTr)) })

    def initHorizontal(a: A, hsh: Tree[Tree[Tree[A, S[S[P]]], S[P]], P], m: M[(B, Tree[Address[S[P]], P])]) : M[(B, Tree[Address[S[P]], P])] = 
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
        case Leaf(ssp) => ???
        case Node(a1, Leaf(sp)) => ???
        case Node(a1, Node(v, hsh1)) => ???
      }

    // initVertial : A → Tree A (suc (suc n)) → Tree (Tree (Tree A (suc (suc n))) (suc n)) n → M (B × Tree (Address (suc n)) n)
    // initVertial a₀ leaf hsh₀ = initHorizontal a₀ hsh₀ (λ-rec [] >>= (λ b → η (b , mapWithAddress hsh₀ (λ _ d → d ∷ []))))
    // initVertial a₀ (node a₁ leaf) hsh₀ = initHorizontal a₀ hsh₀ (ν-rec a₀ leaf >>= (λ b → η (b , const hsh₀ [])))
    // initVertial a₀ (node a₁ (node v hsh₁)) hsh₀ = initHorizontal a₀ hsh₀ (initVertial a₁ v hsh₁)

  }

  //============================================================================================
  // JOIN
  //

  def join[M[+_], A, N <: Nat](tr : Tree[Tree[A, N], N])(implicit sm : ShapeMonad[M]) : M[Tree[A, N]] = ???

  // def join[N <: Nat, A](tr : Tree[N, Tree[N, A]]) : Option[Tree[N, A]] = 
  //   (new NatCaseSplit {

  //     type Out[N <: Nat] = Tree[N, Tree[N, A]] => Option[Tree[N, A]]

  //     def caseZero : Tree[_0, Tree[_0, A]] => Option[Tree[_0, A]] = {
  //       case Pt(t) => Some(t)
  //     }
        
  //     def caseSucc[P <: Nat](p : P) : Tree[S[P], Tree[S[P], A]] => Option[Tree[S[P], A]] = {
  //       case Leaf(d) => Some(Leaf(d))
  //       case Node(t, tsh) => 
  //         for {
  //           gsh <- traverse(tsh)(join(_))
  //           str <- graft(t, gsh)
  //         } yield str
  //     }

  //   })(tr.dim)(tr)

  // //============================================================================================
  // // GRAFT ELIMINATION
  // //

  // trait TreeGraftElim[P <: Nat, A, B] {

  //   def caseLeaf(addr : Address[P]) : Option[B]
  //   def caseNode(a : A, sh : Tree[P, B]) : Option[B]

  //   def apply(tr : Tree[S[P], A]) : Option[B] = 
  //     graftElim(tr)(this)

  // }

  // // All this seems to work as expected.  Just needs to be cleaned up and simplified since it
  // // is really in a performance critical position.  Ideas include a custom traverse which runs
  // // a computation on two trees as they are being zipped and a trait which captures the node
  // // and leaf recursion functions to simplify the API.

  // def graftRecHoriz[N <: Nat, A, B](
  //   elim : TreeGraftElim[S[N], A, B],
  //   addr : Address[S[N]],
  //   deriv : Derivative[N, Address[S[N]]],
  //   tr : Tree[S[N], Tree[S[S[N]], A]]
  // ) : Option[(Tree[S[N], B], Tree[N, Address[S[N]]])] = 
  //   tr match {
  //     case Leaf(d) => Some(Leaf(d), plug(tr.dim.pred)(deriv, addr))
  //     case Node(Leaf(d), hsh) => {

  //       val contData = map(localData[N, Tree[S[N], Tree[S[S[N]], A]], Address[S[N]]](hsh))({
  //         case (dir, deriv, vsh) => (dir :: addr, (deriv, vsh))
  //       })

  //       for { 
  //         res <- graftElimCont(elim, contData)
  //         (bSh , adTr) = res
  //         b <- elim.caseLeaf(addr)
  //       } yield (Node(b, bSh), adTr)
  //     }
  //     case Node(Node(a, vsh), hsh) => 
  //       for {
  //         pr <- graftRecHoriz(elim, addr, deriv, vsh)
  //         (bTr, adTr0) = pr
  //         zt <- matchTree(adTr0,  hsh.zipWithDerivative[Address[S[N]]]) 
  //         res <- graftElimCont(elim, zt)
  //         (bSh, adTr) = res
  //         b <- elim.caseNode(a, bTr)
  //       } yield (Node(b, bSh), adTr)
  //   }

  // def graftElimCont[N <: Nat, A, B](
  //   elim : TreeGraftElim[S[N], A, B],
  //   contData : Tree[N, (Address[S[N]], (Derivative[N, Address[S[N]]], Tree[S[N], Tree[S[S[N]], A]]))]
  // ) : Option[(Tree[N, Tree[S[N], B]], Tree[N, Address[S[N]]])] = 
  //   for {
  //     trRes <- contData.traverse({ case (addr, (deriv, nsh)) => graftRecHoriz(elim, addr, deriv, nsh) })
  //     (bSh, adrJnSh) = unzip(trRes)
  //     adSh <- join(adrJnSh)
  //   } yield (bSh, adSh)

  // def graftElimChain[N <: Nat, A, B](
  //   elim : TreeGraftElim[S[N], A, B],
  //   a : A,
  //   hsh : Tree[N, Tree[S[N], Tree[S[S[N]], A]]],
  //   prevOpt : Option[(B, Tree[N, Address[S[N]]])]
  // ) : Option[(B, Tree[N, Address[S[N]]])] = 
  //   for {
  //     pr0 <- prevOpt
  //     (b0, adTr0) = pr0
  //     zt <- matchTree(adTr0, hsh.zipWithDerivative[Address[S[N]]])
  //     pr <- graftElimCont(elim, zt)
  //     (bSh, adTr) = pr
  //     b <- elim.caseNode(a, Node(b0, bSh))
  //   } yield (b, adTr)

  // def graftElimStart[N <: Nat, A, B](
  //   elim : TreeGraftElim[S[N], A, B], 
  //   tr : Tree[S[S[N]], A],
  //   a : A,
  //   hsh : Tree[N, Tree[S[N], Tree[S[S[N]], A]]]
  // ) : Option[(B, Tree[N, Address[S[N]]])] = 
  //   tr match {
  //     case Leaf(dim) => 
  //       graftElimChain(elim, a, hsh,
  //         for {
  //           b <- elim.caseLeaf(rootAddr(dim.pred))
  //         } yield (b, mapWithAddress(hsh)((_, dir) => dir :: rootAddr(dim.pred)))
  //       )
  //     case Node(a0, Leaf(dim)) => 
  //       graftElimChain(elim, a, hsh,
  //         for {
  //           b <- elim.caseNode(a0, Leaf(dim))
  //         } yield (b, mapWithAddress(hsh)((dir, _) => rootAddr(dim))) 
  //       )
  //     case Node(a0, Node(v, hsh0)) =>
  //       graftElimChain(elim, a, hsh, graftElimStart(elim, v, a0, hsh0))
  //   }

  // def graftElim[N <: Nat, A, B](tr : Tree[S[N], A])(elim : TreeGraftElim[N, A, B]) : Option[B] = 
  //   (new NatCaseSplit {

  //     type Out[N <: Nat] = (Tree[S[N], A], TreeGraftElim[N, A, B]) => Option[B]

  //     def caseZero : Out[_0] = {
  //       case (Leaf(d), elim) => elim.caseLeaf(rootAddr(__0))
  //       case (Node(hd, Pt(tl)), elim) => 
  //         for {
  //           b0 <- caseZero(tl, elim)
  //           b <- elim.caseNode(hd, Pt(b0))
  //         } yield b
  //     }

  //     def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
  //       case (Leaf(d), elim) => elim.caseLeaf(rootAddr(d.pred))
  //       case (Node(a, Leaf(d)), elim) => elim.caseNode(a, Leaf(d))
  //       case (Node(a, Node(v, hsh)), elim) =>
  //         for {
  //           res <- graftElimStart(elim, v, a, hsh)
  //         } yield res._1
  //     }

  //   })(tr.dim.pred)(tr, elim)

  // //============================================================================================
  // // GRAFT
  // //

  // def graft[N <: Nat, A](tr : Tree[S[N], A], brs : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] = 
  //   (new TreeGraftElim[N, A, Tree[S[N], A]] {

  //     def caseLeaf(addr : Address[N]) : Option[Tree[S[N], A]] = 
  //       brs valueAt addr

  //     def caseNode(a : A, sh : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] = 
  //       Some(Node(a, sh))

  //   })(tr)

  // //============================================================================================
  // // FLATTEN
  // //

  // def flatten[N <: Nat, A](ds : Address[S[N]], deriv : Derivative[N, Address[S[N]]], tr : Tree[S[N], A]) : Option[Tree[N, Address[S[N]]]] = 
  //   tr match {
  //     case Leaf(d) => Some(plug(d.pred)(deriv, ds))
  //     case Node(a, sh) => 
  //       for {
  //         jnSh <- traverse(localData[N, Tree[S[N], A], Address[S[N]]](sh))({
  //           case (dir, deriv, v) => flatten[N, A](dir :: ds, deriv, v)
  //         })
  //         res <- join(jnSh)
  //       } yield res

  //   }

  // def flatten[N <: Nat, A](deriv : Derivative[N, Address[S[N]]], tr : Tree[S[N], A]) : Option[Tree[N, Address[S[N]]]] = 
  //   flatten(rootAddr(tr.dim), deriv, tr)

  // def flatten[N <: Nat, A](tr : Tree[S[N], A]) : Option[Tree[N, Address[S[N]]]] = 
  //   flatten(globDerivative(tr.dim.pred), tr)

  // //============================================================================================
  // // TAKE WHILE
  // //

  // def takeWhile[N <: Nat, A](tr : Tree[S[N], A])(p : A => Boolean) : Tree[S[N], A] = 
  //   tr match {
  //     case Leaf(d) => Leaf(d)
  //     case Node(a, sh) => 
  //       if (p(a)) {
  //         Node(a, sh map (takeWhile(_)(p)))
  //       } else {
  //         Leaf(S(sh.dim))
  //       }
  //   }

  // //============================================================================================
  // // EXCISION
  // //

  // def exciseDeriv[N <: Nat, A, B](deriv : Derivative[N, Tree[S[N], A]], tr : Tree[S[N], A], msk : Tree[S[N], B]) : Option[(Tree[S[N], A], Tree[N, Tree[S[N], A]])] =
  //   (tr, msk) match {
  //     case (tr, Leaf(d)) => Some(Leaf(d), plug(d.pred)(deriv, tr))
  //     case (Leaf(_), Node(_, _)) => None
  //     case (Node(a, sh), Node(_, mskSh)) => 
  //       for {
  //         zpSh <- matchTree(sh.zipWithDerivative[Tree[S[N], A]], mskSh) 
  //         zsh <- traverse(zpSh)({
  //           case ((d, t), m) => exciseDeriv(d, t, m)
  //         })
  //         (nsh, crpJn) = unzip(zsh)
  //         crp <- join(crpJn)
  //       } yield (Node(a, nsh), crp)
  //   }

  // def excise[N <: Nat, A, B](tr : Tree[S[N], A], msk : Tree[S[N], B]) : Option[(Tree[S[N], A], Tree[N, Tree[S[N], A]])] =
  //   exciseDeriv(globDerivative(tr.dim.pred), tr, msk)

  // def replace[N <: Nat, A, B](tr : Tree[S[N], A], msk : Tree[S[N], B], rpl : Tree[S[N], A] => A) : Option[Tree[S[N], A]] = 
  //   for {
  //     exPr <- excise(tr, msk)
  //     (exTr, exSh) = exPr
  //   } yield Node(rpl(exTr), exSh)

}

trait TreeImplicits {

  // implicit def treeIsTraverse[N <: Nat] : Traverse[({ type L[+A] = Tree[N, A] })#L] = 
  //   new Traverse[({ type L[+A] = Tree[N, A] })#L] {

  //     override def map[A, B](ta : Tree[N, A])(f : A => B) : Tree[N, B] = 
  //       Tree.map(ta)(f)

  //     def traverseImpl[G[_], A, B](ta : Tree[N, A])(f : A => G[B])(implicit isA : Applicative[G]) : G[Tree[N, B]] = 
  //       Tree.traverse(ta)(f)

  //   }

  // import scalaz.syntax.FunctorOps
  // import scalaz.syntax.functor._

  // implicit def treeToFunctorOps[N <: Nat, A](tr : Tree[N, A]) : FunctorOps[({ type L[+X] = Tree[N, X] })#L, A] = 
  //   ToFunctorOps[({ type L[+X] = Tree[N, X] })#L, A](tr)

  // import scalaz.syntax.TraverseOps
  // import scalaz.syntax.traverse._

  // implicit def treeToTraverseOps[N <: Nat, A](tr : Tree[N, A]) : TraverseOps[({ type L[+X] = Tree[N, X] })#L, A] = 
  //   ToTraverseOps[({ type L[+X] = Tree[N, X] })#L, A](tr)

  // class TreeOps[N <: Nat, A](tr : Tree[N, A]) {

  //   val T = Traverse[({ type L[+A] = Tree[N, A] })#L]

  //   def foreach(op : A => Unit) : Unit = 
  //     Tree.foreach(tr)(op)

  //   def mapWithAddress[B](f : (A, Address[N]) => B) : Tree[N, B] = 
  //     Tree.mapWithAddress(tr)(f)

  //   def zipWithDerivative[B] : Tree[N, (Derivative[N, B], A)] = 
  //     Tree.zipWithDerivative[N, A, B](tr)

  //   def zipWithAddress : Tree[N, (A, Address[N])] = 
  //     Tree.mapWithAddress(tr)((_, _))

  //   def addressTree : Tree[N, Address[N]] =
  //     Tree.mapWithAddress(tr)({ case (_, addr) => addr })

  //   def matchWith[B](trB : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
  //     Tree.matchTree(tr, trB)

  //   def seekTo(addr : Address[N]) : Option[Zipper[N, A]] = 
  //     (new NatCaseSplit {

  //       type Out[N <: Nat] = (Address[N], Tree[N, A]) => Option[Zipper[N, A]]

  //       def caseZero : (Address[_0], Tree[_0, A]) => Option[Zipper[_0, A]] = 
  //         (addr, tr) => Some(tr, ())

  //       def caseSucc[P <: Nat](p : P) : (Address[S[P]], Tree[S[P], A]) => Option[Zipper[S[P], A]] = 
  //         (addr, tr) => Zippers.seek((tr, Nil : Context[S[P], A]), addr)

  //     })(tr.dim)(addr, tr)

  //   def rootOption : Option[A] = 
  //     Tree.rootOption(tr)

  //   def constWith[B](b : B) : Tree[N, B] = 
  //     Tree.map(tr)(_ => b)

  //   def nodes : List[A] = T.toList(tr)
  //   def nodeCount : Int = T.count(tr)
  //   def zipWithIndex : (Int, Tree[N, (A, Int)]) = 
  //     T.mapAccumL(tr, 0)((i : Int, a : A) => (i + 1, (a, i)))

  //   def valueAt(addr : Address[N]) : Option[A] = 
  //     for {
  //       zipper <- tr seekTo addr
  //       a <- zipper.focus.rootOption
  //     } yield a

  // }

  // implicit def treeToTreeOps[N <: Nat, A](tr : Tree[N, A]) : TreeOps[N, A] = 
  //   new TreeOps[N, A](tr)

}

object Tree extends TreeFunctions 
    with TreeImplicits



