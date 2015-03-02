/**
  * Zipper.scala - Zippers for Higher Dimensional Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scalaz.Leibniz._

import Nats._
import Tree._

//============================================================================================
// Derivatives
//

sealed abstract class Derivative[N <: Nat, +A] { def dim : N }
case object ZeroDeriv extends Derivative[_0, Nothing] { val dim = Z }
case class Open[N <: Nat, +A](sh : Tree[N, Tree[S[N], A]], ct : Context[S[N], A]) extends Derivative[S[N], A] { val dim = S(sh.dim) }

object Derivative {

  //============================================================================================
  // PLUG
  //

  def plug[N <: Nat, A](d : Derivative[N, A], a : A) : Tree[N, A] = 
    (new NatCaseSplitTwo {

      type In0[M <: Nat] = Derivative[M, A]
      type In1[M <: Nat] = A
      type Out[M <: Nat] = Tree[M, A]

      def caseZero(d : Derivative[_0, A], a : A) : Tree[_0, A] = Pt(a)

      def caseSucc[P <: Nat](d : Derivative[S[P], A], a : A) : Tree[S[P], A] =
        d match {
          case Open(sh, ct) => Context.close(ct, Node(a, sh))
        }

    })(d.dim, d, a)

  //============================================================================================
  // GLOB DERIVATIVE
  //

  def globDerivative[N <: Nat, B](n : N) : Derivative[N, B] =
    n match {
      case IsZero(zm) => {
        import zm._
        zeroCoe.subst[({ type L[M <: Nat] = Derivative[M, B] })#L](ZeroDeriv)
      }
      case IsSucc(sm) => {
        import sm._

        val prevDeriv = globDerivative[P, Tree[S[P], B]](p)
        val thisDeriv = Open[P, B](plug(prevDeriv, Leaf(S(p))), Empty[S[P]]())

        succCoe.subst[({ type L[M <: Nat] = Derivative[M, B] })#L](thisDeriv)
      }
    }

}


//============================================================================================
// Contexts
//

sealed abstract class Context[N <: Nat, +A] 
case class Empty[N <: Nat]() extends Context[N, Nothing] 
case class Then[N <: Nat, +A](a : A, d : Derivative[N, Tree[S[N], A]], ds : Context[S[N], A]) extends Context[S[N], A] 

object Context {

  //============================================================================================
  // CLOSE
  //

  def close[N <: Nat, A](c : Context[N, A], t : Tree[N, A]) : Tree[N, A] = 
    (new NatCaseSplitTwo {

      type In0[M <: Nat] = Context[M, A]
      type In1[M <: Nat] = Tree[M, A]
      type Out[M <: Nat] = Tree[M, A]

      def caseZero(c : Context[_0, A], t : Tree[_0, A]) : Tree[_0, A] =
        c match {
          case Empty() => t
        }

      def caseSucc[P <: Nat](c : Context[S[P], A], t : Tree[S[P], A]) : Tree[S[P], A] =
        c match {
          case Empty() => t
          case Then(a, d, ds) => caseSucc(ds, Node(a, Derivative.plug(d, t)))
        }

    })(t.dim, c, t)

}

//============================================================================================
// Zippers
//

sealed abstract class Zipper[N <: Nat, +A] {

  def focus : Tree[N, A]
  def context : Context[N, A]

}

case class FocusPoint[+A](pt : Tree[_0, A]) extends Zipper[_0, A] {

  def focus : Tree[_0, A] = pt
  def context : Context[_0, A] = Empty()

}

case class FocusList[+A](lst : Tree[_1, A], ctxt : Context[_1, A]) extends Zipper[_1, A] {

  def focus : Tree[_1, A] = lst
  def context : Context[_1, A] = ctxt

}

case class FocusBranch[N <: Nat, +A](tr : Tree[S[S[N]], A], ctxt : Context[S[S[N]], A]) extends Zipper[S[S[N]], A] {

  def focus : Tree[S[S[N]], A] = tr
  def context : Context[S[S[N]], A] = ctxt

}

object Zipper {

  //============================================================================================
  // VISIT
  //

  def visit[N <: Nat, A](d : Direction[N], z : Zipper[N, A]) : Option[Zipper[N, A]] = 
    (new NatCaseSplit1Two {

      type In0[M <: Nat] = Direction[M]
      type In1[M <: Nat] = Zipper[M, A]
      type Out[M <: Nat] = Option[Zipper[M, A]]

      def caseZero(d : Direction[_0], z : Zipper[_0, A]) : Option[Zipper[_0, A]] =
        Some(z)

      def caseOne(d : Direction[_1], z : Zipper[_1, A]) : Option[Zipper[_1, A]] =
        z match {
          case FocusList(Leaf(_), cntxt) => None
          case FocusList(Node(hd, Pt(tl)), cntxt) => Some(FocusList(tl, Then(hd, ZeroDeriv, cntxt)))
        }

      def caseDblSucc[P <: Nat](d : Direction[S[S[P]]], z : Zipper[S[S[P]], A]) : Option[Zipper[S[S[P]], A]] =
        z match {
          case FocusBranch(Leaf(_), cntxt) => None
          case FocusBranch(Node(a, sh), cntxt) =>
            for {
              shZip <- Tree.seekTo(sh, d)
              resZip <- (
                shZip.focus match {
                  case Leaf(_) => None
                  case Node(t, tsh) => Some(FocusBranch(t, Then(a, Open(tsh, shZip.context), cntxt)))
                }
              )
            } yield resZip
        }

    })(d.dim, d, z)

  //============================================================================================
  // SEEK
  //

  def seek[N <: Nat, A](addr : Address[N], zipper : Zipper[N, A]) : Option[Zipper[N, A]] =
    addr match {
      case Root() => Some(zipper)
      case Step(d, ds) =>
        for {
          zp <- seek(ds, zipper)
          zr <- visit(d, zp)
        } yield zr
    }

}
