/**
  * Nesting.scala - Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

import Nat._
import Tree._
import Zippers._

sealed abstract class Nesting[N <: Nat, +A] { def dim : N }
case class Obj[+A](a : A) extends Nesting[_0, A] { def dim = Z }
case class Dot[N <: Nat, +A](a : A, d : S[N]) extends Nesting[S[N], A] { def dim = d }
case class Box[N <: Nat, +A](a : A, c : Tree[N, Nesting[N, A]]) extends Nesting[N, A] { def dim = c.dim }

trait NestingFunctions { self : NestingImplicits =>

  //============================================================================================
  // MAP
  //

  def mapNesting[N <: Nat, A, B](nst : Nesting[N, A])(f : A => B) : Nesting[N, B] =
    nst match {
      case Obj(a) => Obj(f(a))
      case Dot(a, d) => Dot(f(a), d)
      case Box(a, c) => Box(f(a), c map (mapNesting(_)(f)))
    }

  //============================================================================================
  // TRAVERSE
  //

  def traverseNesting[N <: Nat, T[_], A, B](nst : Nesting[N, A])(f : A => T[B])(implicit apT : Applicative[T]) : T[Nesting[N, B]] = {
    import apT.{pure, ap, ap2}

    nst match {
      case Obj(a) => ap(f(a))(pure(Obj(_)))
      case Dot(a, d) => ap2(f(a), pure(d))(pure(Dot(_, _)))
      case Box(a, c) => ap2(f(a), Tree.traverse(c)(traverseNesting(_)(f)))(pure(Box(_, _)))
    }
  }

  //============================================================================================
  // CASE SPLITTING
  //

  trait NestingCaseSplit[A] {

    type Out[N <: Nat, +U <: Nesting[N, A]]

    def caseObj(a : A) : Out[_0, Obj[A]]
    def caseDot[P <: Nat](a : A, d : S[P]) : Out[S[P], Dot[P, A]]
    def caseBox[N <: Nat](a : A, c : Tree[N, Nesting[N, A]]) : Out[N, Box[N, A]]

    def apply[N <: Nat](nst : Nesting[N, A]) : Out[N, Nesting[N, A]] = 
      caseSplit(nst)(this)

  }

  def caseSplit[N <: Nat, A](nst : Nesting[N, A])(sp : NestingCaseSplit[A]) : sp.Out[N, Nesting[N, A]] = 
    nst match {
      case Obj(a) => sp.caseObj(a)
      case Dot(a, d) => sp.caseDot(a, d)
      case Box(a, c) => sp.caseBox(a, c)
    }

  //============================================================================================
  // BASE VALUE
  //

  def baseValue[N <: Nat, A](nst : Nesting[N, A]) : A =
    nst match {
      case Obj(a) => a
      case Dot(a, _) => a
      case Box(a, _) => a
    }

  //============================================================================================
  // TO TREE
  //

  def toTree[N <: Nat, A](nst : Nesting[N, A]) : Tree[S[N], A] = 
    nst match {
      case Obj(a) => Leaf(__1)
      case Dot(a, d) => Leaf(S(d))
      case Box(a, canopy) => Node(a, map(canopy)(toTree(_)))
    }

  //============================================================================================
  // EXTEND NESTING
  //

  def extendNesting[N <: Nat, A, B](b : B, nst : Nesting[N, A]) : Tree[S[N], Nesting[S[N], B]] = 
    nst match {
      case Obj(a) => Leaf(S(nst.dim))
      case Dot(a, sp) => Leaf(S(sp))
      case Box(a, sh) => Node(Dot(b, S(nst.dim)), Tree.map(sh)(extendNesting(b, _)))
    }

  //============================================================================================
  // SPINE FROM CANOPY
  //

  def spineFromCanopy[N <: Nat, A](canopy : Tree[N, Nesting[N, A]]) : Option[Tree[N, A]] =
    for {
      toJoin <- canopy.zipWithDerivative[A].traverse({
        case (deriv, nst) => spineFromDerivative(deriv, nst)
      })
      result <- join(toJoin)
    } yield result

  //============================================================================================
  // SPINE FROM DERIVATIVE
  //

  def spineFromDerivative[N <: Nat, A](d : Derivative[N, A], nst : Nesting[N, A]) : Option[Tree[N, A]] = 
    (new NestingCaseSplit[A] {

      type Out[N <: Nat, +U <: Nesting[N, A]] = Derivative[N, A] => Option[Tree[N, A]]

      def caseObj(a : A) : Out[_0, Obj[A]] = 
        deriv => Some(Pt(a))

      def caseDot[P <: Nat](a : A, d : S[P]) : Out[S[P], Dot[P, A]] = 
        deriv => Some(plug(d)(deriv, a))

      def caseBox[N <: Nat](a : A, c : Tree[N, Nesting[N, A]]) : Out[N, Box[N, A]] = 
        deriv => spineFromCanopy(c)

    })(nst)(d)

  //============================================================================================
  // WITH ADDRESS
  //

  def withAddress[N <: Nat, A](addr : Address[S[N]], nst : Nesting[N, A]) : Nesting[N, (A, Address[S[N]])] = 
    (new NestingCaseSplit[A] {

      type Out[N <: Nat, +U <: Nesting[N, A]] = Address[S[N]] => Nesting[N, (A, Address[S[N]])]

      def caseObj(a : A) : Out[_0, Obj[A]] = 
        addr => Obj(a, addr)

      def caseDot[P <: Nat](a : A, d : S[P]) : Out[S[P], Dot[P, A]] = 
        addr => Dot((a, addr), d)

      def caseBox[N <: Nat](a : A, c : Tree[N, Nesting[N, A]]) : Out[N, Box[N, A]] = 
        addr => Box((a, addr), mapWithAddress(c)(
          (d, t) => withAddress(d :: addr, t)
        ))

    })(nst)(addr)

  def withAddress[N <: Nat, A](nst : Nesting[N, A]) : Nesting[N, (A, Address[S[N]])] = 
    withAddress(rootAddr(S(nst.dim)), nst)

  //============================================================================================
  // ZIP COMPLETE
  //

  def zipCompleteNesting[N <: Nat, A, B](nstA : Nesting[N, A], nstB : Nesting[N, B]) : Option[Nesting[N, (A, B)]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = (Nesting[N, A], Nesting[N, B]) => Option[Nesting[N, (A, B)]]

      def caseZero : Out[_0] = {
        case (Obj(a), Obj(b)) => Some(Obj((a, b)))
        case (Box(a, cpA), Box(b, cpB)) =>
          for {
            cpAB <- cpA matchWith cpB
            cpRes <- cpAB traverse {
              case (nA, nB) => caseZero(nA, nB)
            }
          } yield Box((a, b), cpRes)
        case _ => None
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (Dot(a, d), Dot(b, _)) => Some(Dot((a, b), d))
        case (Box(a, cpA), Box(b, cpB)) =>
          for {
            cpAB <- cpA matchWith cpB
            cpRes <- cpAB traverse {
              case (nA, nB) => zipCompleteNesting(nA, nB)
            }
          } yield Box((a, b), cpRes)
        case _ => None
      }

    })(nstA.dim)(nstA, nstB)

  //============================================================================================
  // EXTRUDE NESTING
  //

  def extrudeNesting[N <: Nat, A, B](a : A, addr : Address[N], tr : Tree[N, Nesting[N, A]], msk : Tree[N, B]) : Option[Tree[N, Nesting[N, A]]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = (Address[N], Tree[N, Nesting[N, A]], Tree[N, B]) => Option[Tree[N, Nesting[N, A]]]

      def caseZero : Out[_0] = {
        case (addr, cnpy, msk) => Some(Pt(Box(a, cnpy)))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (addr, cnpy, msk) => 
          for {
            zipper <- cnpy seekTo addr 
            newFcs <- replace(zipper.focus, msk, (c : Tree[S[P], Nesting[S[P], A]]) => Box(a, c))
          } yield close(S(p))(zipper.context, newFcs)
      }

    })(tr.dim)(addr, tr, msk)

  //============================================================================================
  // NESTING ZIPPERS
  //

  type NestingDerivative[N <: Nat, +A] = 
    (Tree[N, Nesting[N, A]], NestingContext[N, A])

  type NestingContext[N <: Nat, +A] = 
    List[(A, Derivative[N, Nesting[N, A]])]

  type NestingZipper[N <: Nat, +A] = 
    (Nesting[N, A], NestingContext[N, A])

  def plugNesting[N <: Nat, A](n : N)(deriv : NestingDerivative[N, A], a : A) : Nesting[N, A] = 
    closeNesting(n)(deriv._2, Box(a, deriv._1))

  def closeNesting[N <: Nat, A](n : N)(cntxt : NestingContext[N, A], nst : Nesting[N, A]) : Nesting[N, A] = 
    cntxt match {
      case Nil => nst
      case (a, d) :: cs => closeNesting(n)(cs, Box(a, plug(n)(d, nst)))
    }

  def visitNesting[N <: Nat, A](dir : Address[N], zipper : NestingZipper[N, A]) : Option[NestingZipper[N, A]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = (Address[N], NestingZipper[N, A]) => Option[NestingZipper[N, A]]

      def caseZero : Out[_0] = {
        case (d, (Obj(_), cntxt)) => None
        case (d, (Box(a, Pt(int)), cntxt)) => 
          Some(int, (a, ()) :: cntxt)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (d, (Dot(_, _), cntxt)) => None
        case (d, (Box(a, canopy), cntxt)) => 
          for {
            loc <- canopy seekTo d
            res <- (
              loc.focus match {
                case Leaf(_) => None
                case Node(nst, hsh) => 
                  Some(nst, (a, (hsh, loc.context)) :: cntxt)
              }
            )
          } yield res
      }

    })(zipper._1.dim)(dir, zipper)

  def seekNesting[N <: Nat, A](addr : Address[S[N]], nz : NestingZipper[N, A]) : Option[NestingZipper[N, A]] = 
    addr match {
      case Nil => Some(nz)
      case (d :: ds) =>
        for {
          zp <- seekNesting(ds, nz)
          zr <- visitNesting(d, zp)
        } yield zr
    }

  def sibling[N <: Nat, A](addr : Address[N], nz : NestingZipper[S[N], A]) : Option[NestingZipper[S[N], A]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = (Address[N], NestingZipper[S[N], A]) => Option[NestingZipper[S[N], A]]

      def caseZero : Out[_0] = {
        case (addr, (nst, Nil)) => None
        case (addr, (nst, (a, (Pt(Leaf(d)), hcn)) :: cntxt)) => None
        case (addr, (nst, (a, (Pt(Node(nfcs, sh)), hcn)) :: cntxt)) => 
          Some(nfcs, (a, (sh, (nst, ()) :: hcn)) :: cntxt)
      }

      type UnfoldedZipper[P <: Nat] = 
        (Nesting[S[S[P]], A], List[(A, DerivDblSucc[P, Nesting[S[S[P]], A]])])

      def caseSucc[P <: Nat](p : P) : (Address[S[P]], UnfoldedZipper[P]) => Option[UnfoldedZipper[P]] = {
        case (addr, (nst, Nil)) => None
        case (addr, (nst, (a, (verts, hcn)) :: cntxt)) => 
          for {
            vzip <- verts seekTo addr
            res <- (
              vzip.focus match {
                case Leaf(_) => None
                case Node(Leaf(_), _) => None
                case Node(Node(nfcs, vrem), hmask) => 
                  Some(nfcs, (a, (vrem, (nst, (hmask, vzip.context)) :: hcn)) :: cntxt)
              }
            )
          } yield res
      }

    })(nz._1.dim.pred)(addr, nz)

}

trait NestingImplicits {

  implicit def nestingIsTraverse[N <: Nat] : Traverse[({ type L[+A] = Nesting[N, A] })#L] = 
    new Traverse[({ type L[+A] = Nesting[N, A] })#L] {

      override def map[A, B](na : Nesting[N, A])(f : A => B) : Nesting[N, B] = 
        Nesting.mapNesting(na)(f)

      def traverseImpl[G[_], A, B](na : Nesting[N, A])(f : A => G[B])(implicit isA : Applicative[G]) : G[Nesting[N, B]] = 
        Nesting.traverseNesting(na)(f)

    }

  import scalaz.syntax.FunctorOps
  import scalaz.syntax.functor._

  implicit def nestingToFunctorOps[N <: Nat, A](nst : Nesting[N, A]) : FunctorOps[({ type L[+X] = Nesting[N, X] })#L, A] = 
    ToFunctorOps[({ type L[+X] = Nesting[N, X] })#L, A](nst)

  import scalaz.syntax.TraverseOps
  import scalaz.syntax.traverse._

  implicit def nestingToTraverseOps[N <: Nat, A](nst : Nesting[N, A]) : TraverseOps[({ type L[+X] = Nesting[N, X] })#L, A] = 
    ToTraverseOps[({ type L[+X] = Nesting[N, X] })#L, A](nst)

  class NestingOps[N <: Nat, A](nst : Nesting[N, A]) {

    def zipWithAddress : Nesting[N, (A, Address[S[N]])] = 
      Nesting.withAddress(nst)

    def matchWith[B](nstB : Nesting[N, B]) : Option[Nesting[N, (A, B)]] = 
      Nesting.zipCompleteNesting(nst, nstB)

  }

  implicit def nestingToNestinOps[N <: Nat, A](nst : Nesting[N, A]) : NestingOps[N, A] = 
    new NestingOps[N, A](nst)

}

object Nesting extends NestingFunctions
    with NestingImplicits
