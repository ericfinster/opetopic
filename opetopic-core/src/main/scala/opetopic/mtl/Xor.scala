/**
  * Xor.scala - Exclusive Or
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mtl

sealed abstract class Xor[+A, +B] extends Product with Serializable

trait XorInstances {

  implicit def xorIsMonad[A]: Monad[({ type L[+B] = Xor[A, B] })#L] = 
    new Monad[({ type L[+B] = Xor[A, B] })#L] {

      def pure[U](u: U): Xor[A, U] = Xor.Right(u)

      def flatMap[U, V](xor: Xor[A, U])(f: U => Xor[A, V]): Xor[A, V] = 
        xor match {
          case Xor.Left(a) => Xor.Left(a)
          case Xor.Right(u) => f(u)
        }

    }

}

object Xor extends XorInstances {
  final case class Left[+A](a: A) extends Xor[A, Nothing]
  final case class Right[+B](b: B) extends Xor[Nothing, B]
}

