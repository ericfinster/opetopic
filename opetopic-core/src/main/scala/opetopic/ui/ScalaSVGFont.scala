/**
  * ScalaSVGFont.scala - A Font Description for auto-sized svg output
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

// object FXSVGBundle
//     extends scalatags.Text.Cap
//     with scalatags.text.SvgTags
//     with scalatags.DataConverters
//     with scalatags.Text.Aggregate{ object svgattr extends scalatags.Text.Cap with scalatags.Text.SvgAttrs }

// import FXSVGBundle._

// case class BBox(x: Int, y: Int, width: Int, height: Int) {

//   def coordStr : String = 
//     x.toString ++ " " ++ y.toString ++ " " ++ 
//       width.toString ++ " " ++ height.toString

// }

trait ScalaSVGFont {

  val name: String

  val glyphMap: Map[Char, ScalaSVGFontGlyph]

  // def renderExample(str: String) : Tag = {
  //   import svgattr._

  //   val (pathGrp, bbox) = renderText(str)

  //   val r = rect(
  //     x:=bbox.x.toString,
  //     y:=bbox.y.toString,
  //     width:=bbox.width.toString,
  //     height:=bbox.height.toString,
  //     stroke:="red",
  //     fill:="none"
  //   )

  //   svg(viewBox:=bbox.coordStr,xmlns:="http://www.w3.org/2000/svg")(pathGrp, r)

  // }

  // def renderText(str: String) : (Tag, BBox) = {

  //   import svgattr._

  //   var advance = 0
  //   var ascent = 0
  //   var descent = 0

  //   val paths : Seq[Tag] = (str map (c => {

  //     if (glyphMap.isDefinedAt(c)) {

  //       val glyph = glyphMap(c)

  //       val p = path(d:=glyph.pathStr,transform:="translate(" ++ advance.toString ++") scale(1,-1)",fill:="black",stroke:="black")

  //       advance += glyph.advance
  //       ascent = Math.max(ascent,glyph.ascent)
  //       descent = Math.max(descent,glyph.descent)

  //       Some(p)

  //     } else None

  //   })).flatten

  //   (g(paths), BBox(0, -ascent, advance, ascent + descent))

  // }

  // def renderChar(c: Char) : Tag = 
  //   renderChar(c, 0)

  // def renderChar(c: Char, off: Int) : Tag = {
  //   import svgattr._

  //   if (glyphMap.isDefinedAt(c)) {

  //     val glyph = glyphMap(c)

  //     val p = path(d:=glyph.pathStr,transform:="translate(" ++ off.toString ++") scale(1,-1)",fill:="black",stroke:="black")

  //     val r = rect(
  //       x:="0",
  //       y:=(-glyph.ascent).toString,
  //       width:=glyph.advance,
  //       height:=glyph.height,
  //       stroke:="red",
  //       fill:="none"
  //     )

  //     val s = rect(
  //       x:=glyph.leftBearing,
  //       y:=(-glyph.ascent).toString,
  //       width:=glyph.rightBearing.toString,
  //       height:=glyph.height,
  //       stroke:="green",
  //       fill:="none"
  //     )

  //     val viewboxStr = "0 " ++ (-glyph.ascent).toString ++ " " ++ glyph.advance.toString ++ " " ++ glyph.height.toString

  //     svg(viewBox:=viewboxStr,xmlns:="http://www.w3.org/2000/svg")(p,r,s)

  //   } else {
  //     path()
  //   }

  // }

}

case class ScalaSVGFontGlyph(
  val ascent: Int,
  val descent: Int,
  val advance: Int,
  val leftBearingPos: Int,
  val rightBearingPos: Int,
  val pathStr: String
) {

  def height = ascent + descent

  def leftBearing = leftBearingPos
  def rightBearing = rightBearingPos - leftBearing

}

