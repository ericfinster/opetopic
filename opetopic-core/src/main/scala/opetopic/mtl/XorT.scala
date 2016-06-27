/**
  * XorT.scala - The Xor Transformer
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mtl

final case class XorT[F[_], A, B](val value: F[Xor[A, B]])

object XorT {

  def xorTIsMonad[F[_], A](implicit isM: Monad[F]): Monad[Lambda[B => XorT[F, A, B]]] = 
    new Monad[Lambda[B => XorT[F, A, B]]] {

      def pure[U](u: U): XorT[F, A, U] = XorT(isM.pure(Xor.Right(u)))

      def flatMap[U, V](xor: XorT[F, A, U])(f: U => XorT[F, A, V]): XorT[F, A, V] = 
        xor match {
          case XorT(value) => {

            val r = isM.flatMap[Xor[A, U], Xor[A, V]](value)({
              case Xor.Left(a) => isM.pure(Xor.Left(a))
              case Xor.Right(u) => f(u).value
            })

            XorT(r)

          }
        }

    }

}
