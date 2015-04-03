/**
  * Core.scala - Package Object for Core Package
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds

import scalaz.Monad

package object core 
    extends NatExports
    with AddressExports 
    with ZipperExports {

  trait ShapeMonad[M[+_]] extends Monad[M] {

    def failWith[A](se : ShapeError) : M[A]

  }

}

