/**
  * ScalatagsFramework.scala - Scalatags framework implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scala.math.Ordering._

import scalatags.generic._
import opetopic._

class ScalatagsFramework[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT]) 
    extends UIFramework {

  import bundle._

  type Size = Int
  type Element = TypedTag[Builder, Output, FragT]

  val isNumeric: Numeric[Int] = implicitly[Numeric[Int]]
  val isOrdered: Ordering[Int] = implicitly[Ordering[Int]]

  type PathType = Element
  type TextType = Element
  type GroupType = Element
  type PolygonType = Element
  type RectangleType = Element
  type ViewportType = Element

  def half(i: Int) : Int = i / 2

  def transform(el: Element, t: Transform) : Element = {
    import implicits._

    val translateStr = "translate(" ++ t.translateX.toString ++ ", " ++ t.translateY.toString ++ ")"
    val scaleStr = "scale(" ++ t.scaleX.toString ++ ", " ++ t.scaleY.toString ++ ")"

    el(svgAttrs.transform:=(translateStr ++ " " ++ scaleStr))
  }

  def translate(el: Element, x: Int, y: Int) : Element = {
    import implicits._
    el(svgAttrs.transform:="translate(" ++ x.toString ++ ", " ++ y.toString ++ ")")
  }

  def scale(el: Element, x: Int, y: Int) : Element = {
    import implicits._
    el(svgAttrs.transform:="scale(" ++ x.toString ++ ", " ++ y.toString ++ ")")
  }

  def addClass(el: Element, str: String) : Element = {
    import implicits._
    el(attrs.cls := str)
  }

  def makeMouseInvisible(el: Element) : Element = {
    import implicits._
    el(svgAttrs.pointerEvents:="none")
  }

  def viewport(width: Int, height: Int, bounds: Bounds, elems: Element*) : ViewportType = {
    import svgTags._
    import implicits._

    svg(
      svgAttrs.width := width,
      svgAttrs.height := height,
      svgAttrs.viewBox := bounds.dimString,
      svgAttrs.xmlns := "http://www.w3.org/2000/svg"
    )(elems)

  }

  def group(elems: Element*) : GroupType = {
    import svgTags._
    g(elems)
  }

  def polygon(stroke: String, strokeWidth: Int, fill: String, pts: List[(Int, Int)]) : PolygonType = {
    import implicits._

    svgTags.polygon(
      svgAttrs.points:=(pts map ({ case (x, y) => x.toString ++ "," ++ y.toString })).mkString(" "),
      svgAttrs.stroke:=stroke,
      svgAttrs.strokeWidth:=strokeWidth.toString,
      svgAttrs.fill:=fill
    )

  }

  def rect(x : Int, y: Int, width: Int, height: Int, cornerRadius: Int, stroke: String, strokeWidth: Int, fill: String) : RectangleType = {
    import implicits._

    svgTags.rect(
      svgAttrs.x:=x.toString, 
      svgAttrs.y:=y.toString,
      svgAttrs.width:=width.toString,
      svgAttrs.height:=height.toString,
      svgAttrs.rx:=cornerRadius.toString,
      svgAttrs.ry:=cornerRadius.toString,
      svgAttrs.fill:=fill,
      svgAttrs.stroke:=stroke,
      svgAttrs.strokeWidth:=strokeWidth.toString
    )

  }

  def path(d: String, stroke: String, strokeWidth: Int, fill: String) : PathType = {
    import implicits._

    svgTags.path(
      svgAttrs.d:=d,
      svgAttrs.fill:=fill,
      svgAttrs.stroke:=stroke,
      svgAttrs.strokeWidth:=strokeWidth.toString
    )

  }

  def text(str: String) : BoundedElement = {
    val (g, b) = renderTextGroup(str, AsanaMathMain, "black", 1, "black")
    BoundedElement(g, b)
  }

}

object ScalatagsTextFramework extends ScalatagsFramework(scalatags.Text)
