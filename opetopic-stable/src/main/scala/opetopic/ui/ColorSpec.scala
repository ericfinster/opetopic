/**
  * ColorSpec.scala - Simple Cell Color Specification
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

case class ColorSpec(
  val fill: String,
  val fillHovered: String,
  val fillSelected: String,
  val stroke: String,
  val strokeHovered: String,
  val strokeSelected: String
)

object DefaultColorSpec extends ColorSpec(
  fill = "#FFFFFF",
  fillHovered = "#F3F4F5",
  fillSelected = "#DCDDDE",
  stroke = "#000000",
  strokeHovered = "#000000",
  strokeSelected = "#000000"
)

object PolarityColorSpec extends ColorSpec(
  fill = "#DDDDDD",
  fillHovered = "#DDDDDD",
  fillSelected = "#DDDDDD",
  stroke = "#000000",
  strokeHovered = "#000000",
  strokeSelected = "#000000"
)
