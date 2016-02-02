/**
  * CellMarker.scala - A Class for Labelling Cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import opetopic._
import opetopic.ui._
import opetopic.js._

case class CellMarker[N <: Nat](
  val label: String,
  val colorSpec: ColorSpec = DefaultColorSpec
)

object CellMarker {

  type OptCellMarker[N <: Nat] = Option[CellMarker[N]]

  object ActiveInstance {

    import JsDomFramework._

    implicit object CellMarkerFamily extends VisualizableFamily[CellMarker] {
      def visualize[N <: Nat](n: N)(mk: CellMarker[N]) : Visualization[N] = 
        Visualization(n)(mk.colorSpec, text(mk.label))
    }

  }

  object StaticInstance {

    import opetopic.ui.ScalatagsTextFramework._

    implicit object CellMarkerFamily extends VisualizableFamily[CellMarker] {
      def visualize[N <: Nat](n: N)(mk: CellMarker[N]) : Visualization[N] = 
        Visualization(n)(mk.colorSpec, text(mk.label))
    }

  }

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
      case "grey"   => ("#767676", "#767676", "#767676")
      case "black"  => ("#1B1C1D", "#1B1C1D", "#1B1C1D")
      case _ => ("#FFFFFF", "#F3F4F5", "#DCDDDE")
    }

}

