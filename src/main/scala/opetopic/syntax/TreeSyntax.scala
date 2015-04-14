/**
  * TreeSyntax.scala - Tree specific syntax and operations
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.syntax

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Traverse
import scalaz.Applicative

import opetopic._
import syntax.tree._

final class TreeOps[A, N <: Nat](tr : Tree[A, N]) {

  val T = Traverse[({ type L[+A] = Tree[A, N] })#L]

  def nodes : List[A] = T.toList(tr)
  def nodeCount : Int = T.count(tr)
  def zipWithIndex : (Int, Tree[(A, Int), N]) =
    T.mapAccumL(tr, 0)((i : Int, a : A) => (i + 1, (a, i)))

  //   def foreach(op : A => Unit) : Unit =
  //     Tree.foreach(tr)(op)

  //   def mapWithAddress[B](f : (A, Address[N]) => B) : Tree[N, B] =
  //     Tree.mapWithAddress(tr)(f)

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

  //   def valueAt(addr : Address[N]) : Option[A] =
  //     for {
  //       zipper <- tr seekTo addr
  //       a <- zipper.focus.rootOption
  //     } yield a

}

trait ToTreeOps {

  implicit def treeIsTraverse[N <: Nat] : Traverse[({ type L[+A] = Tree[A, N] })#L] = 
    new Traverse[({ type L[+A] = Tree[A, N] })#L] {

      def traverseImpl[G[_], A, B](ta : Tree[A, N])(f : A => G[B])(implicit isA : Applicative[G]) : G[Tree[B, N]] = 
        Tree.traverse(ta)(f)

    }

  import scalaz.syntax.FunctorOps
  import scalaz.syntax.TraverseOps
  import scalaz.syntax.traverse._

  implicit def treeToTraverseOps[A, N <: Nat](tr : Tree[A, N]) : TraverseOps[({ type L[+X] = Tree[X, N] })#L, A] = 
    ToTraverseOps[({ type L[+X] = Tree[X, N] })#L, A](tr)

  implicit def treeToFunctorOps[A, N <: Nat](tr : Tree[A, N]) : FunctorOps[({ type L[+X] = Tree[X, N] })#L, A] =
    ToFunctorOps[({ type L[+X] = Tree[X, N] })#L, A](tr)

  implicit def treeToTreeOps[A, N <: Nat](tr : Tree[A, N]) : TreeOps[A, N] = 
    new TreeOps[A, N](tr)

}
