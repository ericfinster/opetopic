/**
  * ActiveFramework.scala - A Rendering Framework which provides element permanence
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

abstract class ActiveFramework[U](implicit isNumeric: Numeric[U], isOrdered: Ordering[U]) extends RenderingFramework[U] {

  import isNumeric._

  type UIElementType

  type TextType <: Element with Text
  type PathType <: Element with Path
  type GroupType <: Element with Group
  type RectangleType <: Element with Rectangle

  def transform(el: Element, t: Transform) : Element = 
    { el.transform(t) ; el }

  def translate(el: Element, x: U, y: U) : Element = 
    transform(el, Transform(x, y, fromInt(1), fromInt(1)))

  def scale(el: Element, x: U, y: U) : Element = 
    transform(el, Transform(zero, zero, x, y))

  abstract class Element { thisElement : Element => 

    def uiElement : UIElementType
    def transform(t : Transform) : Unit

  }

  def rect: RectangleType
  def group: GroupType
  def path: PathType

  trait Rectangle { thisRect : Element with RectangleType =>

    var x: U
    var y: U
    var width: U
    var height: U
    var r: U
    var stroke: String
    var strokeWidth: U
    var fill: String

  }

  trait Group { thisGroup : Element with GroupType => 

    var children: Seq[Element]

  }

  trait Path { thisPath : Element with PathType =>

    var d: String
    var stroke: String
    var strokeWidth: U
    var fill: String

  }

  trait Text { thisText : Element with TextType =>

    var text: String
    def bounds: BBox

  }

}
