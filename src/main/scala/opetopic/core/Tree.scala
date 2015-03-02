/**
  * Tree.scala - Higher Dimensional Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._
import scalaz.Leibniz._

import Nats._

sealed abstract class Tree[N <: Nat, +A] { def dim : N }
case class Pt[+A](a : A) extends Tree[_0, A] { def dim = Z }
case class Leaf[N <: Nat](d : S[N]) extends Tree[S[N], Nothing] { def dim = d ; override def toString = "Leaf" }
case class Node[N <: Nat, +A](a : A, shell : Tree[N, Tree[S[N], A]]) extends Tree[S[N], A] { def dim = S(shell.dim) ;  }

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

    def zipWithAddress : Tree[N, (Address[N], A)] = 
      Tree.zipWithAddress(tr)

    def zipWithDerivative[B] : Tree[N, (Derivative[N, B], A)] = 
      Tree.zipWithDerivative[N, A, B](tr)

    def matchWith[B](trB : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
      Tree.zipComplete(tr, trB)

    def seekTo(addr : Address[N]) : Option[Zipper[N, A]] = 
      Tree.seekTo(tr, addr)

    def rootValue : Option[A] = 
      Tree.rootValue(tr)

  }

  implicit def treeToTreeOps[N <: Nat, A](tr : Tree[N, A]) : TreeOps[N, A] = 
    new TreeOps[N, A](tr)

}

trait TreeFunctions { tfns : TreeImplicits => 

  //============================================================================================
  // TYPE PREDICATES
  //

  def isNode[N <: Nat, A](tr : Tree[N, A]) : Boolean = 
    tr match {
      case Pt(_) => true
      case Leaf(_) => false
      case Node(_, _) => true
    }

  def isLeaf[N <: Nat, A](tr : Tree[N, A]) : Boolean = ! isNode(tr)

  //============================================================================================
  // MAP
  //

  def map[N <: Nat, A, B](tr : Tree[N, A])(f : A => B) : Tree[N, B] = 
    tr match {
      case Pt(a) => Pt(f(a))
      case Leaf(d) => Leaf(d)
      case Node(a, sh) => Node(f(a), map(sh)(map(_)(f))) 
    }

  //============================================================================================
  // MAP WITH ADDRESS
  //

  def mapWithAddressFrom[N <: Nat, A, B](tr : Tree[N, A], addr : Address[N])(f : (Address[N], A) => B) : Tree[N, B] = 
    tr match {
      case Pt(a) => Pt(f(addr, a))
      case Leaf(d) => Leaf(d)
      case Node(a, sh) => {
        Node(f(addr, a), mapWithAddress(sh)((dir, brnch) =>
          mapWithAddressFrom(brnch, Step(dir, addr))(f)
        ))
      }
    }

  def mapWithAddress[N <: Nat, A, B](tr : Tree[N, A])(f : (Address[N], A) => B) : Tree[N, B] =
    mapWithAddressFrom(tr, Root()(tr.dim))(f)

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

  def traverseWithAddressFrom[N <: Nat, T[_], A, B](
    tr : Tree[N, A], addr : Address[N]
  )(f : (Address[N], A) => T[B])(implicit apT : Applicative[T]) : T[Tree[N, B]] = {

    import apT.{pure, ap, ap2}
    
    tr match {
      case Pt(a) => ap(f(addr, a))(pure(Pt(_)))
      case Leaf(d) => pure(Leaf(d))
      case Node(a, sh) => ap2(f(addr, a), traverseWithAddress(sh)((dir, brnch) =>
        traverseWithAddressFrom(brnch, Step(dir, addr))(f)
      ))(pure(Node(_, _)))
    }

  }

  def traverseWithAddress[N <: Nat, T[_], A, B](tr : Tree[N, A])(f : (Address[N], A) => T[B])(implicit apT : Applicative[T]) : T[Tree[N, B]] = 
    traverseWithAddressFrom(tr, Root()(tr.dim))(f)

  //============================================================================================
  // TREE RECURSORS
  //

  def treeRecWithAddressFrom[N <: Nat, A, B](tr : Tree[S[N], A], addr : Address[S[N]])(
    leafRec : Address[S[N]] => B,
    nodeRec : (Address[S[N]], A, Tree[N, B]) => B) : B =
    tr match {
      case Leaf(d) => leafRec(addr)
      case Node(a, sh) => nodeRec(addr, a, 
        mapWithAddress(sh)((dir, brnch) =>
          treeRecWithAddressFrom(brnch, Step(dir, addr))(leafRec, nodeRec)
        )
      )
    }

  def treeRecWithAddress[N <: Nat, A, B](tr : Tree[S[N], A])(
    leafRec : Address[S[N]] => B,
    nodeRec : (Address[S[N]], A, Tree[N, B]) => B
  ) : B = treeRecWithAddressFrom(tr, Root()(tr.dim))(leafRec, nodeRec)

  // Again, this is not a real eliminator, but you could certainly beef it
  // up to in fact be one.  You should do that!!!
  // trait TreeElim[A, F[_ <: Nat]] { self => 

  //   def ptElim(a : A) : F[_0]
  //   def lfElim[P <: Nat](p : S[P]) : F[S[P]]
  //   def ndElim[P <: Nat](a : A, sh : Tree[P, F[S[P]]]) : F[S[P]]

  //   def apply[N <: Nat](tr : Tree[N, A]) : F[N] = 
  //     tr match {
  //       case Pt(a) => ptElim(a)
  //       case Leaf(p) => lfElim(p)
  //       case Node(a, sh) => ndElim(a, map(sh)(self(_)))
  //     }

  // }

  //============================================================================================
  // ZIP COMPLETE
  //

  def zipComplete[N <: Nat, A, B](trA : Tree[N, A], trB : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
    (trA, trB) match {
      case (Pt(a), Pt(b)) => Some(Pt((a, b)))
      case (Leaf(addr), Leaf(_)) => Some(Leaf(addr))
      case (Node(a, ash), Node(b, bsh)) =>
        for {
          zsh <- zipComplete(ash, bsh)
          psh <- traverse(zsh)({
            case (at, bt) => zipComplete(at, bt)
          })
        } yield Node((a, b), psh)

      case _ => None
    }

  //============================================================================================
  // ZIP WITH DERIVATIVE
  //

  def zipWithDerivative[N <: Nat, A, B](tr : Tree[N, A]) : Tree[N, (Derivative[N, B], A)] = 
    tr match {
      case Pt(a) => Pt((ZeroDeriv, a))
      case Leaf(d) => Leaf(d)
      case Node(a, sh) => Node(
        (Open(const(sh)(Leaf(tr.dim)), Empty[S[Nat]]), a),
        map(sh)(zipWithDerivative(_))
      )
    }

  //============================================================================================
  // SEEK
  //

  def seekTo[N <: Nat, A](tr : Tree[N, A], addr : Address[N]) : Option[Zipper[N, A]] = 
    (new NatCaseSplit1 {

      type In[M <: Nat] = (Tree[M, A], Address[M])
      type Out[M <: Nat] = Option[Zipper[M, A]]

      def caseZero(pr : (Tree[_0, A], Address[_0])) : Option[Zipper[_0, A]] =
        Zipper.seek(pr._2, FocusPoint(pr._1))

      def caseOne(pr : (Tree[_1, A], Address[_1])) : Option[Zipper[_1, A]] =
        Zipper.seek(pr._2, FocusList(pr._1, Empty()))

      def caseDblSucc[P <: Nat](pr : (Tree[S[S[P]], A], Address[S[S[P]]])) : Option[Zipper[S[S[P]], A]] =
        Zipper.seek(pr._2, FocusBranch(pr._1, Empty()))

    })(tr.dim, (tr, addr))

  //============================================================================================
  // VALUE AT
  //

  def valueAt[N <: Nat, A](tr : Tree[N, A], addr : Address[N]) : Option[A] = 
    for {
      zp <- tr.seekTo(addr)   
      a <- zp.focus.rootValue 
    } yield a

  //============================================================================================
  // ROOT VALUE
  //

  def rootValue[N <: Nat, A](tr : Tree[N, A]) : Option[A] = 
    tr match {
      case Pt(a) => Some(a)
      case Leaf(_) => None
      case Node(a, _) => Some(a)
    }

  //============================================================================================
  // UNZIP
  //

  def unzip[N <: Nat, A, B](tr : Tree[N, (A, B)]) : (Tree[N, A], Tree[N, B]) = 
    (new NatCaseSplit {

      type In[M <: Nat] = Tree[M, (A, B)]
      type Out[M <: Nat] = (Tree[M, A], Tree[M, B])

      def caseZero(t : Tree[_0, (A, B)]) : (Tree[_0, A], Tree[_0, B]) = 
        t match {
          case Pt((a, b)) => (Pt(a), Pt(b))
        }

      def caseSucc[P <: Nat](t : Tree[S[P], (A, B)]) : (Tree[S[P], A], Tree[S[P], B]) = 
        t match {
          case Leaf(sp) => (Leaf(sp), Leaf(sp))
          case Node((a, b), sh) => {
            val (aSh, bSh) = unzip(map(sh)(unzip(_)))
            (Node(a, aSh), Node(b, bSh))
          }
        }

    })(tr.dim, tr)

  //============================================================================================  
  // LOCAL DATA
  //

  def localData[N <: Nat, A, B](tr : Tree[N, A]) : Tree[N, (Address[N], Derivative[N, B], A)] = 
    tr match {
      case Pt(a) => Pt((Root(), ZeroDeriv, a))
      case Leaf(d) => Leaf(d)
      case n @ Node(_, _) =>
        treeRecWithAddress[Nat, A, Tree[S[Nat], (Address[S[Nat]], Derivative[S[Nat], B], A)]](n : Tree[S[Nat], A])(
          (addr) => Leaf(n.dim),
          (addr, a, sh) => Node((addr, Open(const(sh)(Leaf(S(n.dim))), Empty[S[Nat]]), a), sh)
        )
    }

  def extentsData[N <: Nat, A](tr : Tree[N, A]) : Tree[N, (Address[N], Derivative[N, Address[S[N]]], A)] =
    localData[N, A, Address[S[N]]](tr)

  def shellExtentsFrom[N <: Nat, A](shell : Tree[N, Tree[S[N], A]], addr : Address[S[N]]) : Option[Tree[N, Address[S[N]]]] =
    for {
      rec <- extentsData(shell).traverse({
        case (dir, deriv, Leaf(_)) => Some(Derivative.plug(deriv, Step(dir, addr)))
        case (dir, deriv, Node(_, sh)) => shellExtentsFrom(sh, Step(dir, addr))
      })
      res <- join(rec)
    } yield res


  def shellExtents[N <: Nat, A](shell : Tree[N, Tree[S[N], A]]) : Option[Tree[N, Address[S[N]]]] = 
    shellExtentsFrom(shell, Root()(S(shell.dim)))

  //============================================================================================
  // UTILITIES
  //

  // Note: you should simply move these guys to the tree ops section.  I think all of them will
  // really work better there, and the traversals will really be much nicer ....

  def zipWithAddress[N <: Nat, A](tr : Tree[N, A]) : Tree[N, (Address[N], A)] = 
    mapWithAddress(tr)((_, _))

  def addressTree[N <: Nat, A](tr : Tree[N, A]) : Tree[N, Address[N]] = 
    mapWithAddress(tr)({ case (addr, _) => addr })

  def const[N <: Nat, A, B](tr : Tree[N, A])(b : B) : Tree[N, B] = 
    map(tr)(_ => b)

  // Ugggh.  Can we do better with the traversal implicit ... ???

  def nodesOf[N <: Nat, A](tr : Tree[N, A]) : List[A] = 
    implicitly[Traverse[({ type L[+X] = Tree[N, X] })#L]].toList(tr)

  def nodeCountOf[N <: Nat, A](tr : Tree[N, A]) : Int = 
    implicitly[Traverse[({ type L[+X] = Tree[N, X] })#L]].count(tr)

  def zipWithIndex[N <: Nat, A](tr : Tree[N, A]) : (Int, Tree[N, (A, Int)]) = 
    implicitly[Traverse[({ type L[+X] = Tree[N, X] })#L]].
      mapAccumL(tr, 0)((i : Int, a : A) => (i + 1, (a, i)))

  //============================================================================================
  // GRAFT RECURSION
  //

  trait GraftRecursor[N <: Nat, A, B] {

    def leafRec(addr : Address[N]) : Option[B]
    def nodeRec(a : A, sh : Tree[N, B]) : Option[B]

  }

  // All this seems to work as expected.  Just needs to be cleaned up and simplified since it
  // is really in a performance critical position.  Ideas include a custom traverse which runs
  // a computation on two trees as they are being zipped and a trait which captures the node
  // and leaf recursion functions to simplify the API.

  def graftRecHoriz[N <: Nat, A, B](
    rec : GraftRecursor[S[N], A, B],
    addr : Address[S[N]],
    deriv : Derivative[N, Address[S[N]]],
    tr : Tree[S[N], Tree[S[S[N]], A]]
  ) : Option[(Tree[S[N], B], Tree[N, Address[S[N]]])] = 
    tr match {
      case Leaf(d) => {
        // println("Horizontal leaf")
        Some((Leaf(d), Derivative.plug(deriv, addr)))
      }
      case Node(Leaf(d), hsh) => {

        // println("Passing leaf with address: " ++ addr.toString)

        val contData = map(localData[N, Tree[S[N], Tree[S[S[N]], A]], Address[S[N]]](hsh))({
          case (dir, deriv, vsh) => (Step(dir, addr), (deriv, vsh))
        })

        for { 
          res <- graftRecCont(rec, contData)
          (bSh , adTr) = res
          b <- rec.leafRec(addr)
        } yield (Node(b, bSh), adTr)
      }
      case Node(Node(a, vsh), hsh) => {
        // println("Picking off a vertical branch")
        for {
          pr <- graftRecHoriz(rec, addr, deriv, vsh)
          (bTr, adTr0) = pr
          zt <- zipComplete(adTr0,  hsh.zipWithDerivative[Address[S[N]]]) 
          res <- graftRecCont(rec, zt)
          (bSh, adTr) = res
          b <- rec.nodeRec(a, bTr)
        } yield (Node(b, bSh), adTr)
      }
    }

  def graftRecCont[N <: Nat, A, B](
    rec : GraftRecursor[S[N], A, B],
    contData : Tree[N, (Address[S[N]], (Derivative[N, Address[S[N]]], Tree[S[N], Tree[S[S[N]], A]]))]
  ) : Option[(Tree[N, Tree[S[N], B]], Tree[N, Address[S[N]]])] = 
    for {
      trRes <- contData.traverse({ case (addr, (deriv, nsh)) => graftRecHoriz(rec, addr, deriv, nsh) })
      (bSh, adrJnSh) = unzip(trRes)
      adSh <- join(adrJnSh)
    } yield (bSh, adSh)

  def graftRecChain[N <: Nat, A, B](
    rec : GraftRecursor[S[N], A, B],
    a : A,
    hsh : Tree[N, Tree[S[N], Tree[S[S[N]], A]]],
    prevOpt : Option[(B, Tree[N, Address[S[N]]])]
  ) : Option[(B, Tree[N, Address[S[N]]])] = 
    for {
      pr0 <- prevOpt
      (b0, adTr0) = pr0
      zt <- zipComplete(adTr0, hsh.zipWithDerivative[Address[S[N]]])
      pr <- graftRecCont(rec, zt)
      (bSh, adTr) = pr
      b <- rec.nodeRec(a, Node(b0, bSh))
    } yield (b, adTr)

  def graftRecStart[N <: Nat, A, B](
    rec : GraftRecursor[S[N], A, B], 
    tr : Tree[S[S[N]], A],
    a : A,
    hsh : Tree[N, Tree[S[N], Tree[S[S[N]], A]]]
  ) : Option[(B, Tree[N, Address[S[N]]])] = 
    tr match {
      case Leaf(dim) => 
        graftRecChain(rec, a, hsh,
          for {
            b <- rec.leafRec(Root()(dim.pred))
          } yield (b, mapWithAddress(hsh)((dir, _) => Step(dir, Root()(dim.pred))))
        )
      case Node(a0, Leaf(dim)) => 
        graftRecChain(rec, a, hsh,
          for {
            b <- rec.nodeRec(a0, Leaf(dim))
          } yield (b, mapWithAddress(hsh)((dir, _) => Step(dir, Root()(dim))))
        )
      case Node(a0, Node(v, hsh0)) => 
        graftRecChain(rec, a, hsh, graftRecStart(rec, v, a0, hsh0))
    }

  def graftRec[N <: Nat, A, B](
    rec : GraftRecursor[N, A, B],
    tr : Tree[S[N], A]
  ) : Option[B] =
    (new NatCaseSplitTwo {

      type In0[M <: Nat] = Tree[S[M], A]
      type In1[M <: Nat] = GraftRecursor[M, A, B]
      type Out[M <: Nat] = Option[B]

      def caseZero(t : Tree[_1, A], r : GraftRecursor[_0, A, B]) : Option[B] =
        t match {
          case Leaf(d) => r.leafRec(Root()(Z))
          case Node(hd, Pt(tl)) =>
            for {
              b0 <- caseZero(tl, r)
              b <- r.nodeRec(hd, Pt(b0))
            } yield b
        }

      def caseSucc[P <: Nat](t : Tree[S[S[P]], A], r : GraftRecursor[S[P], A, B]) : Option[B] =
        t match {
          case Leaf(dim) => r.leafRec(Root()(dim.pred))
          case Node(a, Leaf(dim)) => r.nodeRec(a, Leaf(dim))
          case Node(a, Node(v, hsh)) =>
            for {
              res <- graftRecStart(r, v, a, hsh)
            } yield res._1
        }

    })(tr.dim.pred, tr, rec)

  def graft[N <: Nat, A, B](tr : Tree[S[N], A], brs : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] = {
    graftRec[N, A, Tree[S[N], A]](
      new GraftRecursor[N, A, Tree[S[N], A]] {

        def leafRec(addr : Address[N]) : Option[Tree[S[N], A]] = valueAt(brs, addr)
        def nodeRec(a : A, sh : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] = Some(Node(a, sh))

      }, 
      tr
    )
  }

  //============================================================================================
  // JOIN
  //

  def join[N <: Nat, A](tr : Tree[N, Tree[N, A]]) : Option[Tree[N, A]] = 
    (new NatCaseSplit { thisJoin =>

      type In[M <: Nat] = Tree[M, Tree[M, A]]
      type Out[M <: Nat] = Option[Tree[M, A]]

      def caseZero(t : Tree[_0, Tree[_0, A]]) : Option[Tree[_0, A]] = 
        t match {
          case Pt(p) => Some(p)
        }

      def caseSucc[P <: Nat](t : Tree[S[P], Tree[S[P], A]]) : Option[Tree[S[P], A]] = 
        t match {
          case Leaf(sp) => Some(Leaf(sp))
          case Node(t, tsh) =>
            for {
              gsh <- traverse(tsh)(join(_))
              str <- graft(t, gsh)
            } yield str
        }

    })(tr.dim, tr)

  //============================================================================================
  // FLATTEN
  //

  def flattenWithPrefix[N <: Nat, A](
    ds : Address[S[N]], 
    derivOpt : Option[Derivative[N, Address[S[N]]]], 
    tr : Tree[S[N], A]
  ) : Option[Tree[N, Address[S[N]]]] = {

    import Derivative._

    (derivOpt, tr) match {
      case (None, Leaf(_)) => Some(plug(globDerivative[N, Address[S[N]]](tr.dim.pred), ds))
      case (Some(deriv), Leaf(_)) => Some(plug(deriv, ds))
      case (_, Node(a, sh)) => 
        for {
          jnSh <- traverse(localData[N, Tree[S[N], A], Address[S[N]]](sh))({
            case (dir, deriv, v) => flattenWithPrefix(Step(dir, ds), Some(deriv), v)
          })
          res <- join(jnSh)
        } yield res
    }
  }

  def flatten[N <: Nat, A](tr : Tree[S[N], A]) : Option[Tree[N, Address[S[N]]]] = 
    flattenWithPrefix(Root()(tr.dim), None, tr)

  //============================================================================================
  // EXCISION
  //

  def exciseDeriv[N <: Nat, A, B](deriv : Derivative[N, Tree[S[N], A]], tr : Tree[S[N], A], msk : Tree[S[N], B]) : Option[(Tree[S[N], A], Tree[N, Tree[S[N], A]])] =
    (tr, msk) match {
      case (Leaf(sp), Leaf(_)) => Some(Leaf(sp), Derivative.plug(deriv, Leaf(sp)))
      case (Leaf(_), Node(_, _)) => None
      case (Node(a, sh), Leaf(sp)) => Some(Leaf(sp), Derivative.plug(deriv, Leaf(sp)))
      case (Node(a, sh), Node(_, mskSh)) => 
        for {
          zpSh <- zipComplete(sh.zipWithDerivative[Tree[S[N], A]], mskSh) 
          zsh <- traverse(zpSh)({
            case ((d, t), m) => exciseDeriv(d, t, m)
          })
          (nsh, crpJn) = unzip(zsh)
          crp <- join(crpJn)
        } yield (Node(a, nsh), crp)
    }

  def excise[N <: Nat, A, B](tr : Tree[S[N], A], msk : Tree[S[N], B]) : Option[(Tree[S[N], A], Tree[N, Tree[S[N], A]])] =
    exciseDeriv(Derivative.globDerivative(tr.dim.pred), tr, msk)

  def replace[N <: Nat, A, B](tr : Tree[S[N], A], msk : Tree[S[N], B], rpl : Tree[S[N], A] => A) : Option[Tree[S[N], A]] = 
    for {
      exPr <- excise(tr, msk)
      (exTr, exSh) = exPr
    } yield Node(rpl(exTr), exSh)

}

object Tree extends TreeFunctions 
    with TreeImplicits

