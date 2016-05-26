/**
  * ScalaSVGFont.scala - A Font Description for auto-sized svg output
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

trait ScalaSVGFont {

  val name: String
  val glyphMap: Map[Char, ScalaSVGFontGlyph]

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

