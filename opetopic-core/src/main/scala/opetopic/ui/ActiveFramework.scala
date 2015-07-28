/**
  * ActiveFramework.scala - A Framework which admits user interaction
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

abstract class ActiveFramework extends UIFramework {

  import isNumeric._

  //============================================================================================
  // EVENTS
  //

  trait UIEvent 

  trait UIMouseEvent extends UIEvent {

    def button : Int
    def altKey : Boolean
    def shiftKey : Boolean
    def ctrlKey : Boolean

  }

  trait UIKeyEvent extends UIEvent {

    def keyCode : Int
    def altKey : Boolean
    def shiftKey : Boolean
    def ctrlKey : Boolean

  }

  //============================================================================================
  // UI ELEMENTS
  //

  type UIElementType

  type TextType <: Element with Text
  type PathType <: Element with Path
  type GroupType <: Element with Group
  type ViewportType <: Element with Viewport
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

    var onClick : UIMouseEvent => Unit
    var onMouseOut : UIMouseEvent => Unit
    var onMouseOver : UIMouseEvent => Unit
    var onKeyDown : UIKeyEvent => Unit

  }

  def rect: RectangleType
  def group: GroupType
  def path: PathType
  def viewport: ViewportType

  trait Viewport { thisView : Element with ViewportType =>

    var width: Size
    var height: Size

    var viewX : Size
    var viewY : Size
    var viewWidth : Size
    var viewHeight : Size

    var children : Seq[Element]

    def setBounds(bounds: Bounds) : Unit = {
      viewX = bounds.x
      viewY = bounds.y
      viewWidth = bounds.width
      viewHeight = bounds.height
    }

  }

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
