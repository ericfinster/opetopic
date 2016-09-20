/**
  * Traverse.scala - Generic Traverse
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mtl

import scala.collection.immutable.Queue

trait Traverse[F[_]] extends Functor[F] {

  def map[A, B](fa: F[A])(f: A => B): F[B] = 
    traverse[Id, A, B](fa)(f)

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit isAp: Applicative[G]) : G[F[B]]

  //
  //  TODO: Should you worry about the stack?
  //

  import State._

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type L[C] = State[S, C] })#L, A, B](fa)(f)(State.stateIsMonad)

  def toList[A](fa: F[A]): List[A] = 
    exec(traverseS[Queue[A], A, Unit](fa)((a: A) => modify(q => q.enqueue(a))))(Queue()).toList

  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B = 
    exec(traverseS[B, A, Unit](fa)((a: A) => modify(b => f(a, b))))(z)

  def mapAccumL[S, A, B](fa: F[A], z: S)(f: (S, A) ⇒ (S, B)): (S, F[B]) = 
    run(traverseS[S, A, B](fa)((a: A) => (s: S) => f(s, a)))(z)

  def sequence[G[_], A](fga: F[G[A]])(implicit isAp: Applicative[G]): G[F[A]] = 
    traverse(fga)((ga : G[A]) => ga)

}

object Traverse {

  def apply[F[_]](implicit t: Traverse[F]) : Traverse[F] = t

}

abstract class TraverseOps[F[_], A](fa: F[A]) {

  val T: Traverse[F]

  def traverse[G[_], B](f: A => G[B])(implicit isAp: Applicative[G]) : G[F[B]] = 
    T.traverse(fa)(f)

  def map[B](f: A => B): F[B] = 
    T.map(fa)(f)

  def toList: List[A] = 
    T.toList(fa)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = 
    T.foldRight(fa, z)(f)

  def mapAccumL[S, B](z: S)(f: (S, A) ⇒ (S, B)): (S, F[B]) = 
    T.mapAccumL(fa, z)(f)

}

