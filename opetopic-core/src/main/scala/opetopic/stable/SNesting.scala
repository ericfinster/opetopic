/**
  * SNesting.scala - Stable Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

sealed trait SNesting[+A] 
case class SDot[+A](a: A) extends SNesting[A]
case class SBox[+A](a: A, cn: STree[SNesting[A]]) extends SNesting[A]


