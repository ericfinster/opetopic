/**
  * Monad.scala - Monads
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mtl

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  def join[A](ffa: F[F[A]]): F[A] = 
    flatten(ffa)

  override def ap[A, B](fa: F[A])(f: F[(A) => B]): F[B] =
    flatMap(f)(f => map(fa)(f))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

}

object Monad {

  def apply[F[_]](implicit m: Monad[F]): Monad[F] = m

}

abstract class MonadOps[F[_], A](fa: F[A]) {

  val M: Monad[F]

  def map[B](f: A => B): F[B] = 
    M.map(fa)(f)

  def flatMap[B](f: A => F[B]): F[B] = 
    M.flatMap(fa)(f)

}
