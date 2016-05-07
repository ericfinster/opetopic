/**
  * SComplex.scala - Stable Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

sealed trait SComplex[+A]
case class SBase[+A](hd: SNesting[A]) extends SComplex[A]
case class SNext[+A](tl: SComplex[A], hd: SNesting[A]) extends SComplex[A]
