/**
  * NestingZipper.scala - Zippers for Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import Nats._
import Tree._
import Nesting._

//============================================================================================
// NESTING DERIVATIVES
//

case class NestingDerivative[N <: Nat, +A](val canopy : Tree[N, Nesting[N, A]], val context : NestingContext[N, A]) {

  def plugWith[B >: A](b : B) : Nesting[N, B] = context.closeWith(Box(b, canopy))

}

//============================================================================================
// NESTING CONTEXTS
//

sealed abstract class NestingContext[N <: Nat, +A] {

  def closeWith[B >: A](nst : Nesting[N, B]) : Nesting[N, B] = 
    this match {
      case Bottom() => nst
      case Select(a, d, c) => c.closeWith(Box(a, Derivative.plug(d, nst)))
    }

}

case class Bottom[N <: Nat]() extends NestingContext[N, Nothing]
case class Select[N <: Nat, +A](a : A, d : Derivative[N, Nesting[N, A]], c : NestingContext[N, A]) extends NestingContext[N, A]

//============================================================================================
// NESTING ZIPPERS
//

case class NestingZipper[N <: Nat, +A](val focus : Nesting[N, A], val context : NestingContext[N, A]) {

  def dim : N = focus.dim

  def close : Nesting[N, A] = 
    context.closeWith(focus)

  def withFocus[B >: A](fcs : Nesting[N, B]) : NestingZipper[N, B] = 
    NestingZipper(fcs, context)

  def withContext[B >: A](cntxt : NestingContext[N, B]) : NestingZipper[N, B] =
    NestingZipper(focus, cntxt)

}

object NestingZipper {

  //============================================================================================
  // VISIT
  //

  def visit[N <: Nat, A](dir : Direction[S[N]], nz : NestingZipper[N, A]) : Option[NestingZipper[N, A]] = 
    (new NatCaseSplitTwo {

      type In0[M <: Nat] = Direction[S[M]]
      type In1[M <: Nat] = NestingZipper[M, A]
      type Out[M <: Nat] = Option[NestingZipper[M, A]]

      def caseZero(dir : Direction[_1], nz : NestingZipper[_0, A]) =
        nz match {
          case NestingZipper(Obj(_), cntxt) => None
          case NestingZipper(Box(a, Pt(int)), cntxt) =>
            Some(NestingZipper(int, Select(a, ZeroDeriv, cntxt)))
        }

      def caseSucc[P <: Nat](dir : Direction[S[S[P]]], nz : NestingZipper[S[P], A]) : Option[NestingZipper[S[P], A]] =
        nz match {
          case NestingZipper(Dot(_, _), cntxt) => None
          case NestingZipper(Box(a, canopy), cntxt) =>
            for {
              loc <- canopy.seekTo(dir)
              res <- (
                loc.focus match {
                  case Leaf(_) => None
                  case Node(nst, hsh) =>
                    Some(NestingZipper(nst, Select(a, Open(hsh, loc.context), cntxt)))
                }
              )
            } yield res
        }

    })(nz.dim, dir, nz)

  //============================================================================================
  // SIBLING
  //

  def sibling[N <: Nat, A](addr : Address[N], nz : NestingZipper[S[N], A]) : Option[NestingZipper[S[N], A]] = 
    (new NatCaseSplitTwo {

      type In0[M <: Nat] = Address[M]
      type In1[M <: Nat] = NestingZipper[S[M], A]
      type Out[M <: Nat] = Option[NestingZipper[S[M], A]]

      def caseZero(addr : Address[_0], nz : NestingZipper[_1, A]) : Option[NestingZipper[_1, A]] =
        nz match {
          case NestingZipper(fcs, Bottom()) => None
          case NestingZipper(fcs, Select(a, Open(Pt(Leaf(_)), hcn), c)) => None
          case NestingZipper(fcs, Select(a, Open(Pt(Node(nfcs, shell)), hcn), c)) =>
            Some(NestingZipper(nfcs, Select(a, Open(shell, Then(fcs, ZeroDeriv, hcn)), c)))
        }

      def caseSucc[P <: Nat](addr : Address[S[P]], nz : NestingZipper[S[S[P]], A]) : Option[NestingZipper[S[S[P]], A]] =
        nz match {
          case NestingZipper(fcs, Bottom()) => None
          case NestingZipper(fcs, Select(a, Open(verts, hcn), c)) =>
            for {
              vzip <- verts.seekTo(addr)
              res <- (
                vzip.focus match {
                  case Leaf(_) => None
                  case Node(Leaf(_), _) => None
                  case Node(Node(nfcs, vrem), hmask) =>
                    Some(NestingZipper(nfcs, Select(a, Open(vrem, Then(fcs, Open(hmask, vzip.context), hcn)), c)))
                }
              )
            } yield res
        }

    })(addr.dim, addr, nz)

  //============================================================================================
  // SEEK
  //

  def seek[N <: Nat, A](addr : Address[S[N]], nz : NestingZipper[N, A]) : Option[NestingZipper[N, A]] = 
    addr match {
      case Root() => Some(nz)
      case Step(d, ds) =>
        for {
          zp <- seek(ds, nz)
          zr <- visit(d, zp)
        } yield zr
    }

}


