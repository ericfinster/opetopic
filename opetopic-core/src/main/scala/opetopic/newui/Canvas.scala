/**
  * Canvas.scala - An abstract canvas which we can render to
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.newui

abstract class Canvas[U : Numeric] {

  def clear : Unit

  def rect(x: U, y: U, width: U, height: U, r: U) : Unit
  def text(x: U, y: U, txt: String) : Unit

}
