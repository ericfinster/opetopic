/**
  * VisualFramework.scala - An abstraction of a visual framework
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import TypeDefs._

import syntax.nesting._

abstract class VisualFramework {

  abstract class Element

  abstract class Component {
    def render: Element
  }

  abstract class Rectangle extends Component
  abstract class Text extends Component
  abstract class Path extends Component

  abstract class Group extends Component

  def group(children: List[Component]) : Group
  def rect(x: Double, y: Double, width: Double, height: Double) : Rectangle

}

trait PanelFramework { thisFramework : VisualFramework =>

  abstract class Panel[U : Numeric, N <: Nat] extends Component {

    // The idea is that there is a nesting of *components*

    def nesting: Nesting[Component, N]

    def render: Element =
      thisFramework.group(nesting.nodes).render

  }

}
