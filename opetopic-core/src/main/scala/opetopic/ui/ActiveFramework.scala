/**
  * ActiveFramework.scala - A Framework which admits user interaction
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

abstract class ActiveFramework extends UIFramework {

  import isNumeric._

  type UIElementType
  type UIEventType

  type TextType <: Element with Text
  type PathType <: Element with Path
  type GroupType <: Element with Group
  type RectangleType <: Element with Rectangle

  def transform(el: Element, t: Transform) : Element = 
    { el.transform(t) ; el }

  def translate(el: Element, x: Size, y: Size) : Element = 
    transform(el, Transform(x, y, fromInt(1), fromInt(1)))

  def scale(el: Element, x: Size, y: Size) : Element = 
    transform(el, Transform(zero, zero, x, y))

  abstract class Element { 

    def uiElement : UIElementType

    def transform(t : Transform) : Unit

    var onClick : UIEventType => Unit
    var onMouseOut : UIEventType => Unit
    var onMouseOver : UIEventType => Unit

  }

  def rect: RectangleType
  def group: GroupType
  def path: PathType

  trait Rectangle { thisRect : Element with RectangleType =>

    var x: Size
    var y: Size
    var width: Size
    var height: Size
    var r: Size
    var stroke: String
    var strokeWidth: Size
    var fill: String

  }

  trait Group { thisGroup : Element with GroupType => 

    var children: Seq[Element]

  }

  trait Path { thisPath : Element with PathType =>

    var d: String
    var stroke: String
    var strokeWidth: Size
    var fill: String

  }

  trait Text { thisText : Element with TextType =>

    var text: String
    def bounds: Bounds

  }

}
