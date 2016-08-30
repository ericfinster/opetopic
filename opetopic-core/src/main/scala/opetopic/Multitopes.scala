/**
  * Multitopes.scala - An implementation of multitopes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

sealed trait Multitope[+A]
case class Base[+A](c: SComplex[A]) extends Multitope[A]
case class Up[+A](c: SComplex[Multitope[A]]) extends Multitope[A]

object Multitope {

  def level[A](m: Multitope[A]): Int =
    m match {
      case Base(c) => 1
      case Up(c) => level(c.topCell) + 1
    }

  def dim[A](m: Multitope[A]): Int =
    m match {
      case Base(c) => c.dim
      case Up(c) => c.dim * dim(c.topCell)
    }

}

