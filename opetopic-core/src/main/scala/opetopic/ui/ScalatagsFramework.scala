/**
  * ScalatagsFramework.scala - Backend for Scalatags Generation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scala.math.Ordering._

import scalatags.generic._
import opetopic._

class ScalatagsFramework[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT]) 
    extends StaticGalleryFramework[Int] {

  import bundle._

  type Element = TypedTag[Builder, Output, FragT]

  type PathType = Element
  type TextType = Element
  type GroupType = Element
  type RectangleType = Element

  val defaultPanelConfig =
    PanelConfig(
      internalPadding = 200,
      externalPadding = 400,
      halfLeafWidth = 50,
      halfStrokeWidth = 30,
      cornerRadius = 100
    )

  val defaultGalleryConfig = 
    GalleryConfig(defaultPanelConfig, 800)

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

  def group(elems: Element*) : GroupType = {
    import svgTags._
    g(elems)
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

  def text(str: String) : BoundedElement[TextType] = 
    renderTextGroup(str, AsanaMathMain, "black", 1, "black")

}

object ScalatagsTextFramework extends ScalatagsFramework(scalatags.Text)
