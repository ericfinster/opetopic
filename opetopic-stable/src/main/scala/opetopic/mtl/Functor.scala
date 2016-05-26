/**
  * Functor.scala - Functors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mtl

trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

}
