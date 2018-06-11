/**
  * PaperFramework.scala - Implementation of an ActiveFramework for Paper.js
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.editor

import org.scalajs.dom._
import opetopic.ui._
import paperjs._

class PaperFramework extends ActiveFramework {

  type Size = Double

  val isNumeric: Numeric[Double] = implicitly[Numeric[Double]]
  val isOrdered: Ordering[Double] = implicitly[Ordering[Double]]

  def half(u: Double) : Double = u / 2

  type UIElementType = Items.Item

  type PathType = PaperPath
  type TextType = PaperText
  type GroupType = PaperGroup
  type PolygonType = PaperPolygon
  type ViewportType = PaperViewport
  type RectangleType = PaperRectangle

  //
  //  Abstract UI Elements
  //

  abstract class PaperElement extends Element {

    def transform(t : Transform) : Unit = {
      uiElement.translate(Basic.Point(t.translateX, t.translateY))
      uiElement.scale(t.scaleX, t.scaleY)
    }

    var onClick : UIMouseEvent => Unit = { _ => () }
    var onMouseOut : UIMouseEvent => Unit = { _ => () }
    var onMouseOver : UIMouseEvent => Unit = { _ => () }
    var onKeyDown : UIKeyEvent => Unit = { _ => () }

  }

  //
  // Paths
  //

  class PaperPath extends PaperElement with Path {

    private var myPath = new Paths.Path()

    def uiElement = myPath

    def d: String = myPath.pathData
    def d_=(x$1: String): Unit = {

      // Not sure the best way to accomplish this ...
      ???
    }

    def fill: String = myPath.fillColor.toString
    def fill_=(fc: String): Unit = {
      myPath.fillColor = new Styling.Color(fc)
    }

    def strokeWidth: Size = ???
    def strokeWidth_=(x$1: Size): Unit = ???

    def stroke: String = ???
    def stroke_=(x$1: String): Unit = ???

  }

  // Text
  class PaperText extends PaperElement with Text

  // Group
  class PaperGroup extends PaperElement with Group

  // Polygon
  class PaperPolygon extends PaperElement with Polygon

  // Viewport
  class PaperViewport extends PaperElement with Viewport

  // Rectangle
  class PaperRectangle extends PaperElement with Rectangle

}
