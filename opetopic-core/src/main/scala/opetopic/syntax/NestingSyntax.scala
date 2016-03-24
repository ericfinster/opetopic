/**
  * NestingSyntax.scala - Syntax and Operations on Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.syntax

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Id._
import scalaz.Traverse
import scalaz.Applicative

import opetopic._
import syntax.tree._

final class NestingOps[A, N <: Nat](nst: Nesting[A, N]) {

  def toTree : Tree[A, S[N]] = 
    Nesting.toTree(nst)

  def foreach(op: A => Unit) : Unit = 
    Nesting.foreach(nst)(op)

  def traverseWithAddress[G[_], B](f: (A, Address[S[N]]) => G[B])(implicit apG: Applicative[G]) = 
    Nesting.traverseWithAddress(nst)(f)

  def mapWithAddress[B](f: (A, Address[S[N]]) => B) : Nesting[B, N] =
    Nesting.traverseWithAddress[Id, A, B, N](nst)(f)

  def seekTo(addr: Address[S[N]]) : ShapeM[NestingZipper[A, N]] = 
    Nesting.seekNesting(nst.dim)((nst, Nil), addr)

  def baseValue : A = 
    Nesting.baseValue(nst)

  def nodes : List[A] = 
    Nesting.elimWithAddress(nst.dim)(nst)({
      case (a, _) => List(a)
    })({
      case (a, _, cn) => (List(a) :: cn.nodes).flatten
    })

  def replaceAt(addr: Address[S[N]], a: A) : ShapeM[Nesting[A, N]] = 
    Nesting.replaceNesting(nst.dim)(nst, addr, a)

}

trait ToNestingOps {

  implicit def nestingIsTraverse[N <: Nat] : Traverse[({ type L[+A] = Nesting[A, N] })#L] = 
    new Traverse[({ type L[+A] = Nesting[A, N] })#L] {

      def traverseImpl[G[_], A, B](nst : Nesting[A, N])(f : A => G[B])(implicit isA : Applicative[G]) : G[Nesting[B, N]] = 
        Nesting.traverse(nst)(f)

    }

  import scalaz.syntax.FunctorOps
  import scalaz.syntax.TraverseOps
  import scalaz.syntax.traverse._

  implicit def nestingToTraverseOps[A, N <: Nat](nst: Nesting[A, N]) : TraverseOps[({ type L[+X] = Nesting[X, N] })#L, A] = 
    ToTraverseOps[({ type L[+X] = Nesting[X, N] })#L, A](nst)

  implicit def nestingToFunctorOps[A, N <: Nat](nst: Nesting[A, N]) : FunctorOps[({ type L[+X] = Nesting[X, N] })#L, A] = 
    ToFunctorOps[({ type L[+X] = Nesting[X, N] })#L, A](nst)

  implicit def toNestingOps[A, N <: Nat](nst: Nesting[A, N]) : NestingOps[A, N] = 
    new NestingOps(nst)

}
