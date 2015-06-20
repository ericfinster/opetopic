/**
  * Component.scala - An abstract component for the opetopic ui
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.newui

abstract class Component[ElementType] {

  // There should be some kind of state and event
  // setup here as well ....

  def render : ElementType

}

abstract class Text[ElementType] extends Component[ElementType]
abstract class Rectangle[ElementType] extends Component[ElementType]
abstract class Path[ElementType] extends Component[ElementType]

