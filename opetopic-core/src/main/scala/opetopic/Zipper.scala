/**
  * Zipper.scala - Derivatives and Zippers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Monad
import scalaz.syntax.monad._

import TypeDefs._

trait ZipperFunctions {

  @natElim
  def rootAddr[N <: Nat](n: N) : Address[N] = {
    case Z => ()
    case S(p) => Nil
  }

  @natElim
  def plug[A, N <: Nat](n: N)(deriv: Derivative[A, N], a: A) : Tree[A, N] = {
    case (Z, deriv, a) => Pt(a)
    case (S(p), (sh, cs), a) => close(S(p))(cs, Node(a, sh))
  }

  @natElim
  def close[A, N <: Nat](n: N)(cs: Context[A, N], tr: Tree[A, N]) : Tree[A, N] = {
    case (Z, cs, tr) => tr
    case (S(p), Nil, tr) => tr
    case (S(p), (a, d) :: cs, tr) => close(S(p))(cs, Node(a, plug(p)(d, tr)))
  }

  @natElim
  def visit[A, N <: Nat](n : N)(zp : Zipper[A, S[N]], dir : Address[N]) : ShapeM[Zipper[A, S[N]]] = {
    case (Z, (Leaf(_), _), ()) => fail("Cannot visit a leaf")
    case (Z, (Node(hd, Pt(tl)), c), ()) => succeed((tl, (hd, ()) :: c))
    case (S(p: P), (Leaf(_), _), _) => fail("Cannot visit a leaf")
    case (S(p: P), (Node(a, sh), c), d) => 
      for {
        shZp <- seek[Tree[A, S[S[P]]], S[P]](S(p))((sh, Nil), d)
        res <- (
          shZp match {
            case (Leaf(_), _) => fail("Ran out of room in shell")
            case (Node(tr, hsh), c0) => succeed((tr, (a, (hsh, c0)) :: c))
          }
        )
      } yield res
  }

  @natElim
  def seek[A, N <: Nat](n : N)(zp : Zipper[A, N], addr : Address[N]) : ShapeM[Zipper[A, N]] = {
    case (Z, zp, _) => succeed(zp)
    case (S(p), zp, Nil) => succeed(zp)
    case (S(p), zp, d :: ds) =>
      for {
        zpP <- seek(S(p))(zp, ds)
        zpN <- visit(p)(zpP, d)
      } yield zpN
  }

  @natElim
  def parentWhich[A, N <: Nat](n : N)(zp : Zipper[A, N])(f : A => Boolean) : Option[Zipper[A, N]] = {
    case (Z, zp, f) => None
    case (S(p), (fcs, Nil), f) => None
    case (S(p), (fcs, (a, d) :: cs), f) => {
      val parent = (Node(a, plug(p)(d, fcs)), cs)
      if (f(a)) Some(parent) else parentWhich(S(p))(parent)(f)
    }
  }

  @natElim
  def globDerivative[A, N <: Nat](n : N) : Derivative[A, N] = {
    case Z => ()
    case S(p) => (plug(p)(globDerivative(p), Leaf(S(p))), Nil)
  }

}

object Zipper extends ZipperFunctions
