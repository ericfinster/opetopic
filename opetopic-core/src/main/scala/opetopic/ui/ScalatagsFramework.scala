/**
  * ScalatagsFramework.scala - Backend for Scalatags Generation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scalatags.generic._
import opetopic._

class ScalatagsFramework[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT]) 
    extends RenderingFramework[Int] with PanelFramework[Int] {

  import bundle._

  type Element = TypedTag[Builder, Output, FragT]

  def translate(el: Element, x: Int, y: Int) : Element = {
    import implicits._
    el(svgAttrs.transform:="translate(" ++ x.toString ++ ", " ++ y.toString ++ ")")
  }

  def group(elems: Element*) : Element = {
    import svgTags._
    g(elems)
  }

  def rect(x : Int, y: Int, width: Int, height: Int, cornerRadius: Int, strokeWidth: Int) : Element = {
    import implicits._

    svgTags.rect(
      svgAttrs.x:=x.toString, 
      svgAttrs.y:=y.toString,
      svgAttrs.width:=width.toString,
      svgAttrs.height:=height.toString,
      svgAttrs.rx:=cornerRadius.toString,
      svgAttrs.ry:=cornerRadius.toString,
      svgAttrs.fill:="white",
      svgAttrs.stroke:="black",
      svgAttrs.strokeWidth:=strokeWidth.toString
    )

  }

  def path(d: String, strokeWidth: Int) : Element = {
    import implicits._

    svgTags.path(
      svgAttrs.d:=d,
      svgAttrs.fill:="none",
      svgAttrs.stroke:="black",
      svgAttrs.strokeWidth:=strokeWidth.toString
    )
  }

  def boundedText(str: String) : (Element, BBox) = {

    import svgAttrs._
    import implicits._

    var advance = 0
    var ascent = 0
    var descent = 0

    val glyphMap = AsanaMathMain.glyphMap

    val paths : Seq[Element] = (str map (c => {

      if (glyphMap.isDefinedAt(c)) {

        val glyph = glyphMap(c)

        val p = svgTags.path(d:=glyph.pathStr,transform:="translate(" ++ advance.toString ++") scale(1,-1)",fill:="black",stroke:="black")

        advance += glyph.advance
        ascent = Math.max(ascent,glyph.ascent)
        descent = Math.max(descent,glyph.descent)

        Some(p)

      } else None

    })).flatten

    val bbox = new BBox {

      def x = 0
      def y = -ascent
      def halfWidth = advance / 2
      def halfHeight = (ascent + descent) / 2

    }

    (svgTags.g(paths), bbox)

  }

}

object ScalatagsTextFramework extends ScalatagsFramework(scalatags.Text)
