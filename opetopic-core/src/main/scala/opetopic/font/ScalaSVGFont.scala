/**
  * ScalaSVGFont.scala - A Font Description for auto-sized svg output
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.font

trait ScalaSVGFont {
  val name: String
  val glyphMap: Map[String, ScalaSVGFontGlyph]
}

case class ScalaSVGFontGlyph(
  val arg0: Int,
  val arg1: Int,
  val arg2: Int,
  val arg3: Int,
  val arg4: Int,
  val pathStr: String
)
