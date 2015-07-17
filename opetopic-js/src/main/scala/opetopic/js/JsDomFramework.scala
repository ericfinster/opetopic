/**
  * JsDomFramework.scala - Dom Backend for Opetopic Rendering
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs._
import org.scalajs.dom._

import org.scalajs.dom.raw.SVGElement
import org.scalajs.dom.raw.SVGGElement
import org.scalajs.dom.raw.SVGPathElement
import org.scalajs.dom.raw.SVGRectElement
import org.scalajs.dom.raw.SVGTransformable

import opetopic.ui._

object JsDomFramework extends ActiveFramework[Int] with PanelFramework[Int] with ActivePanelFramework[Int] {

  val svgns = "http://www.w3.org/2000/svg"

  val defaultPanelConfig =
    PanelConfig(
      internalPadding = 200,
      externalPadding = 400,
      halfLeafWidth = 50,
      halfStrokeWidth = 30,
      cornerRadius = 100
    )

  def half(u: Int) : Int = u / 2

  type UIElementType = SVGElement

  type PathType = JsDomPath
  type TextType = JsDomText
  type GroupType = JsDomGroup
  type RectangleType = JsDomRectangle

  abstract class JsDomElement extends Element {

    def transform(t: Transform) = {
      // There may be a better way to do this with the dom api ...
      val translateStr = "translate(" ++ t.translateX.toString ++ ", " ++ t.translateY.toString ++ ")"
      val scaleStr = "scale(" ++ t.scaleX.toString ++ ", " ++ t.scaleY.toString ++ ")"
      uiElement.setAttributeNS(null, "transform", translateStr ++ " " ++ scaleStr)
    }

  }

  //============================================================================================
  // RECTANGLES
  //

  def rect : RectangleType = 
    new JsDomRectangle

  def rect(x: Int, y: Int, width: Int, height: Int, r: Int, stroke: String, strokeWidth: Int, fill: String) : RectangleType =
    new JsDomRectangle(x, y, width, height, r, stroke, strokeWidth, fill)

  class JsDomRectangle extends JsDomElement with Rectangle {

    def this(x: Int, y: Int, width: Int, height: Int, r: Int, stroke: String, strokeWitdth: Int, fill: String) = {
      this ; this.x = x ; this.y = y ; this.width = width ; this.height = height
      this.r = r ; this.stroke = stroke ; this.strokeWidth = strokeWidth ; this.fill = fill
    }

    val svgRect = 
      document.createElementNS(svgns, "rect").
        asInstanceOf[SVGRectElement]

    val uiElement = svgRect

    def x: Int = svgRect.getAttributeNS(svgns, "x").toInt
    def x_=(i : Int): Unit = svgRect.setAttributeNS(null, "x", i.toString)

    def y: Int = svgRect.getAttributeNS(svgns, "y").toInt
    def y_=(i : Int): Unit = svgRect.setAttributeNS(null, "y", i.toString)

    def width: Int = svgRect.getAttributeNS(svgns, "width").toInt
    def width_=(i : Int): Unit = svgRect.setAttributeNS(null, "width", i.toString)

    def height: Int = svgRect.getAttributeNS(svgns, "height").toInt
    def height_=(i : Int): Unit = svgRect.setAttributeNS(null, "height", i.toString)

    def r: Int = svgRect.getAttributeNS(svgns, "rx").toInt
    def r_=(i: Int) = {
      svgRect.setAttributeNS(null, "rx", i.toString)
      svgRect.setAttributeNS(null, "ry", i.toString)
    }

    def stroke: String = svgRect.getAttributeNS(svgns, "stroke")
    def stroke_=(s: String): Unit = svgRect.setAttributeNS(null, "stroke", s)

    def strokeWidth: Int = svgRect.getAttributeNS(svgns, "stroke-width").toInt
    def strokeWidth_=(i : Int): Unit = svgRect.setAttributeNS(null, "stroke-width", i.toString)

    def fill: String = svgRect.getAttributeNS(svgns, "fill")
    def fill_=(s: String): Unit = svgRect.setAttributeNS(null, "fill", s)

  }

  //============================================================================================
  // GROUPS
  //

  def group : GroupType = 
    new JsDomGroup 

  def group(els: Element*) : GroupType = 
    new JsDomGroup(els)

  class JsDomGroup extends JsDomElement with Group {

    def this(els: Seq[Element]) = {
      this ; children = els
    }

    val svgGroup =
      document.createElementNS(svgns, "g").
        asInstanceOf[SVGGElement]

    val uiElement = svgGroup

    private var myChildren: Seq[Element] = Seq()

    def children: Seq[Element] = myChildren
    def children_=(els: Seq[Element]) : Unit = {
      var last = svgGroup.lastChild

      while (last != null) { 
        svgGroup.removeChild(last)
        last = svgGroup.lastChild
      }

      for { el <- els } {
        svgGroup.appendChild(el.uiElement) 
      }

      myChildren = els
    }

  }

  //============================================================================================
  // PATHS
  //

  def path : PathType = 
    new JsDomPath

  def path(d: String, stroke: String, strokeWidth: Int, fill: String) : PathType = 
    new JsDomPath(d, stroke, strokeWidth, fill)

  class JsDomPath extends JsDomElement with Path {

    def this(d: String, stroke: String, strokeWidth: Int, fill: String) = {
      this ; this.d = d ; this.stroke = stroke ; this.strokeWidth = strokeWidth ; this.fill = fill
    }

    val svgPath =
      document.createElementNS(svgns, "path").
        asInstanceOf[SVGPathElement]

    val uiElement = svgPath

    def d: String = svgPath.getAttributeNS(svgns, "d")
    def d_=(s: String): Unit = svgPath.setAttributeNS(null, "d", s)

    def stroke: String = svgPath.getAttributeNS(svgns, "stroke")
    def stroke_=(s: String): Unit = svgPath.setAttributeNS(null, "stroke", s)

    def strokeWidth: Int = svgPath.getAttributeNS(svgns, "stroke-width").toInt
    def strokeWidth_=(i : Int): Unit = svgPath.setAttributeNS(null, "stroke-width", i.toString)

    def fill: String = svgPath.getAttributeNS(svgns, "fill")
    def fill_=(s: String): Unit = svgPath.setAttributeNS(null, "fill", s)

  }


  //============================================================================================
  // TEXT
  //

  def text(str: String) : BoundedElement[TextType] = {
    new BoundedElement[TextType] {
      val element = new JsDomText(str)
      val bounds = element.bounds
    }
  }

  class JsDomText extends JsDomElement with Text {

    def this(str: String) = {
      this ; this.text = str
    }

    def uiElement = myGroup.svgGroup

    private var myText: String = ""
    private var myBounds: BBox = BBox(0,0,0,0)
    private var myGroup: JsDomGroup = null

    def text: String = myText 
    def text_=(s: String) = {
      val be = renderTextGroup(s, AsanaMathMain, "black", 1, "black")
      myGroup = be.element
      myBounds = be.bounds

      // If there are event handlers attached to this text element, 
      // they will get lost when you update the text, I think.
      // Watch out for this ...

    }

    def bounds: BBox = myBounds

  }



}
