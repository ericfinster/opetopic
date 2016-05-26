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

}
