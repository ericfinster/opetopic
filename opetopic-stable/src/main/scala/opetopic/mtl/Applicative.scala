/**
  * Applicative.scala - Applicative Functors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mtl

trait Applicative[F[_]] extends Functor[F] {

  def pure[A](a: A): F[A]
  def ap[A, B](fa: F[A])(f: F[(A) => B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = 
    ap(fa)(pure(f))

 def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(fb)(map(fa)(a => (b: B) => (a, b)))

  def ap2[A, B, C](fa: F[A], fb: F[B])(ff: F[(A, B) => C]): F[C] =
    map(product(fa, product(fb, ff))) { case (a, (b, f)) => f(a, b) }

}
