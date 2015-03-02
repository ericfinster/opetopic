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

import Nats._
import Tree._

sealed abstract class Nesting[N <: Nat, +A] { def dim : N }
case class Obj[+A](a : A) extends Nesting[_0, A] { def dim = Z }
case class Dot[N <: Nat, +A](a : A, d : S[N]) extends Nesting[S[N], A] { def dim = d }
case class Box[N <: Nat, +A](a : A, c : Tree[N, Nesting[N, A]]) extends Nesting[N, A] { def dim = c.dim }

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
    (new NatCaseSplitTwo {

      type In0[M <: Nat] = Derivative[M, A]
      type In1[M <: Nat] = Nesting[M, A]
      type Out[M <: Nat] = Option[Tree[M, A]]

      def caseZero(d : Derivative[_0, A], nst : Nesting[_0, A]) : Option[Tree[_0, A]] =
        nst match {
          case Obj(a) => Some(Pt(a))
          case Box(a, canopy) => spineFromCanopy(canopy)
        }

      def caseSucc[P <: Nat](deriv : Derivative[S[P], A], nst : Nesting[S[P], A]) : Option[Tree[S[P], A]] =
        nst match {
          case Dot(a, d) => Some(Derivative.plug(deriv, a))
          case Box(a, canopy) => spineFromCanopy(canopy)
        }

    })(nst.dim, d, nst)

  //============================================================================================
  // WITH ADDRESS
  //

  def withAddressPrefix[N <: Nat, A](addr : Address[S[N]], nst : Nesting[N, A]) : Nesting[N, (A, Address[S[N]])] =
    (new NatCaseSplitTwo {

      type In0[M <: Nat] = Address[S[M]]
      type In1[M <: Nat] = Nesting[M, A]
      type Out[M <: Nat] = Nesting[M, (A, Address[S[M]])]

      def caseZero(addr : Address[_1], nst : Nesting[_0, A]) : Nesting[_0, (A, Address[_1])] =
        nst match {
          case Obj(a) => Obj((a, addr))
          case Box(a, canopy) => {
            Box((a, addr), canopy.zipWithAddress map ({
              case (d, t) => withAddressPrefix(Step(d, addr), t)
            }))
          }
        }

      def caseSucc[P <: Nat](addr : Address[S[S[P]]], nst : Nesting[S[P], A]) : Nesting[S[P], (A, Address[S[S[P]]])] =
        nst match {
          case Dot(a, cor) => Dot((a, addr), cor)
          case Box(a, canopy) => {
            Box((a, addr), canopy.zipWithAddress map ({
              case (d, t) => withAddressPrefix(Step(d, addr), t)
            }))
          }
        }

    })(nst.dim, addr, nst)

  def withAddress[N <: Nat, A](nst : Nesting[N, A]) : Nesting[N, (A, Address[S[N]])] = 
    withAddressPrefix(Root()(S(nst.dim)), nst)

  //============================================================================================
  // ZIP COMPLETE
  //

  def zipCompleteNesting[N <: Nat, A, B](nstA : Nesting[N, A], nstB : Nesting[N, B]) : Option[Nesting[N, (A, B)]] =
    (new NatCaseSplitTwo {

      type In0[M <: Nat] = Nesting[M, A]
      type In1[M <: Nat] = Nesting[M, B]
      type Out[M <: Nat] = Option[Nesting[M, (A, B)]]

      def caseZero(nstA : Nesting[_0, A], nstB : Nesting[_0, B]) : Option[Nesting[_0, (A, B)]] =
        (nstA, nstB) match {
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

      def caseSucc[P <: Nat](nstA : Nesting[S[P], A], nstB : Nesting[S[P], B]) : Option[Nesting[S[P], (A, B)]] =
        (nstA, nstB) match {
          case (Dot(a, d), Dot(b, _)) => Some(Dot((a, b), d))
          case (Box(a, cpA), Box(b, cpB)) =>
            for {
              cpAB <- cpA matchWith cpB
              cpRes <- cpAB traverse {
                case (nA, nB) => caseSucc(nA, nB)
              }
            } yield Box((a, b), cpRes)
          case _ => None
        }

    })(nstA.dim, nstA, nstB)

  //============================================================================================
  // EXTRUDE NESTING
  //

  def extrudeNesting[N <: Nat, A, B](a : A, addr : Address[N], tr : Tree[N, Nesting[N, A]], msk : Tree[N, B]) : Option[Tree[N, Nesting[N, A]]] = 
    (new NatCaseSplit {

      type In[M <: Nat] = (Address[M], Tree[M, Nesting[M, A]], Tree[M, B])
      type Out[M <: Nat] = Option[Tree[M, Nesting[M, A]]]

      def caseZero(trpl : (Address[_0], Tree[_0, Nesting[_0, A]], Tree[_0, B])) : Option[Tree[_0, Nesting[_0, A]]] = 
        Some(Pt(Box(a, trpl._2)))

      def caseSucc[P <: Nat](trpl : (Address[S[P]], Tree[S[P], Nesting[S[P], A]], Tree[S[P], B])) : Option[Tree[S[P], Nesting[S[P], A]]] = {
        val (addr, tr, msk) = trpl

        for {
          zp <- seekTo(tr, addr)
          newFcs <- replace(zp.focus, msk, (c : Tree[S[P], Nesting[S[P], A]]) => Box(a, c))
        } yield Context.close(zp.context, newFcs)
      }

    })(tr.dim, (addr, tr, msk))

}

object Nesting extends NestingFunctions
    with NestingImplicits
