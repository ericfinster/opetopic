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
  import isFractional._

  type ElementType 

  abstract class Component {

    def render: Seq[ElementType]

  }

  //============================================================================================
  // VIEW TRAIT AND TYPE CLASS
  //

  trait View[A] {

    def value: A
    def component: Component

    var x : U
    var y : U

    def width : U
    def height : U

  }

  // A type class for types which can be represented visually
  // in the given framework ...
  trait Viewable[A] {
    def view(a: A) : View[A]
  }

  object Viewable {

    // Hmm. Well, this is clearly not what you want.
    implicit object stringViewable extends Viewable[String] {

      def view(str: String) : View[String] = 
        new View[String] {

          def value: String = str
          def component: Component = Text(zero, zero, str)

          var x: U = zero
          var y: U = zero

          def width: U = zero
          def height: U = zero

        }

    }

  }

  //============================================================================================
  // CONTRUCTORS
  //

  def rect(x: U, y: U, width: U, height: U) : Rectangle
  def text(x: U, y: U, txt: String) : Text

  //============================================================================================
  // CANVAS
  //

  abstract class Canvas extends Component

  // The idea here is that this guy will consist of a size and the ability to 
  // change the viewport in such a way that we can view the nesting boxes which
  // we are going to put inside.

  //============================================================================================
  // RECTANGLE
  //

  abstract class Rectangle extends Component

  object Rectangle {

    def apply(x: U, y: U, width: U, height: U) : Rectangle = 
      rect(x, y, width, height)

  }

  //============================================================================================
  // TEXT
  //

  abstract class Text extends Component

  object Text {

    def apply(x: U, y: U, txt: String) : Text = 
      text(x, y, txt)

  }

  //============================================================================================
  // PATHS
  //

  abstract class Path extends Component

}
