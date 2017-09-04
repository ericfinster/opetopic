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
  val strokeSelected: String,
  val edgeHovered: String
) {

  // Some default colors to play with ...
  def colorTripleGen(color: String) : (String, String, String) =
    color match {
      case "red"    => ("#DB2828", "#DB2828", "#DB2828")
      case "orange" => ("#F2711C", "#F2711C", "#F2711C")
      case "yellow" => ("#FBBD08", "#FBBD08", "#FBBD08")
      case "olive"  => ("#B5CC18", "#B5CC18", "#B5CC18")
      case "green"  => ("#21BA45", "#21BA45", "#21BA45")
      case "teal"   => ("#00B5AD", "#00B5AD", "#00B5AD")
      case "blue"   => ("#2185D0", "#2185D0", "#2185D0")
      case "violet" => ("#6435C9", "#6435C9", "#6435C9")
      case "purple" => ("#A333C8", "#A333C8", "#A333C8")
      case "pink"   => ("#E03997", "#E03997", "#E03997")
      case "brown"  => ("#A5673F", "#A5673F", "#A5673F")
      case "grey"   => ("lightgrey", "darkgrey", "grey")
      case "black"  => ("#1B1C1D", "#1B1C1D", "#1B1C1D")
      case _ => ("#FFFFFF", "#F3F4F5", "#DCDDDE")
    }

  def colorReverseLookup(str: String) : String = 
    str match {
      case "#DB2828" => "red"
      case "#F2711C" => "orange"
      case "#FBBD08" => "yellow"
      case "#B5CC18" => "olive"
      case "#21BA45" => "green"
      case "#00B5AD" => "teal"
      case "#2185D0" => "blue"
      case "#6435C9" => "violet"
      case "#A333C8" => "purple"
      case "#E03997" => "pink"
      case "#A5673F" => "brown"
      case "lightgrey" => "grey"
      case "#1B1C1D" => "black"
      case "#000000" => "black"
      case _ => "white"
    }

  def strokeColor: String = 
    colorReverseLookup(stroke)

  def fillColor: String = 
    colorReverseLookup(fill)

  def withFill(fc: String): ColorSpec = {
    val (f, fh, fs) = colorTripleGen(fc)
    this.copy(fill = f, fillHovered = fh, fillSelected = fs)
  }

  def withStroke(sc: String): ColorSpec = {
    val (s, sh, ss) = colorTripleGen(sc)
    this.copy(stroke = s, strokeHovered = sh, strokeSelected = ss)
  }

}

object DefaultColorSpec extends ColorSpec(
  fill = "#FFFFFF",
  fillHovered = "#DB2828", // "#F3F4F5",
  fillSelected = "#DCDDDE",
  stroke = "#000000",
  strokeHovered = "#000000",
  strokeSelected = "#000000",
  edgeHovered = "#f19091"
)

object PolarityColorSpec extends ColorSpec(
  fill = "#DDDDDD",
  fillHovered = "#DDDDDD",
  fillSelected = "#DDDDDD",
  stroke = "#000000",
  strokeHovered = "#000000",
  strokeSelected = "#000000",
  edgeHovered = "#f19091"
)


