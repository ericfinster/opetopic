/**
  * ShapeMonad.scala - Shape Monads
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds

import scalaz.Monad

trait ShapeMonad[M[+_]] extends Monad[M] {
  def failWith[A](se: ShapeError) : M[A]
  def fromOpt[A](opt: Option[A]) : M[A]
}
