/**
  * SCardinal.scala - Experiments with Cardinals
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

case class SCard[+A](val tree: STree[SCard[A]])


