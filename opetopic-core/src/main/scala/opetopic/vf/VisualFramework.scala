/**
  * VisualFramework.scala - An abstraction of a visual framework
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.vf

import opetopic._
import TypeDefs._

import syntax.nesting._

abstract class VisualFramework[U : Fractional] {

  val isFractional = implicitly[Fractional[U]]

  type ElementType 

  // Hmmm. There should really be a better way of dealing with
  // multiple items in the return ...
  abstract class Component {
    def render: Seq[ElementType]
  }

  abstract class Representation[A] extends Component {

    def value : A

    var x : U
    var y : U

    def width : U
    def height : U

  }

  // A type class for types which can be represented visually
  // in the given framework ...
  trait Representable[A] {
    def repr(a: A) : Representation[A]
  }

  abstract class Rectangle extends Component
  abstract class Text extends Component
  abstract class Path extends Component

  def rect(x: U, y: U, width: U, height: U) : Rectangle

}
