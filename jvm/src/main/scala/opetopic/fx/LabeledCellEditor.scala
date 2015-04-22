/**
  * LabeledCellEditor.scala - A Simple Cardinal Editor for Generating Colored Diagrams
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx

import javafx.scene.Node
import javafx.scene.text.Text
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle

import opetopic._
import TypeDefs._

case class ColoredLabel[N <: Nat](val label: String, val color: Color) {

  def withLabel(str: String) : ColoredLabel[N] = 
    ColoredLabel(str, color)

  def withColor(clr: Color) : ColoredLabel[N] = 
    ColoredLabel(label, clr)

}

object ColoredLabel {

  type LabelOpt[N <: Nat] = Option[ColoredLabel[N]]

  implicit object coloredLabelRenderable extends FXRenderable[ColoredLabel] {
    def render[N <: Nat](n: N)(cl: ColoredLabel[N]) : Node = 
      new Text(cl.label)
  }

}

class LabeledCellEditor extends FXCardinalEditor[ColoredLabel] {

  type NeutralBoxType[N <: Nat] = LabeledCellBox[N]

  def createNeutralBox[N <: Nat](mk : FXNeutralMarker[N]) : LabeledCellBox[N] = 
    new LabeledCellBox(mk)

  class LabeledCellBox[N <: Nat](mk : FXNeutralMarker[N]) extends FXNeutralBox[N] {

    def marker : FXNeutralMarker[N] = mk
    def color : Color =
      marker.label match {
        case Neutral(Some(cl)) => cl.color
        case Neutral(None) => Color.WHITE
        case Positive() => Color.GAINSBORO
        case Negative() => Color.GAINSBORO
      }

  }

}

class LabeledCellViewer(lc: FiniteComplex[ColoredLabel.LabelOpt]) extends FXComplexViewer[ColoredLabel.LabelOpt](lc) {

  type BoxType = ColoredBox

  def createBox[N <: Nat](mk: FXComplexMarker[N]) : BoxType =
    new ColoredBox {
      type Dim = N
      def marker = mk
    }

  abstract class ColoredBox extends FXBox {

    val r = implicitly[FXRenderable[ColoredLabel.LabelOpt]]

    val label : Node = {
      marker.label match {
        case Some(cl) => {
          println("Rendering cl with label: " ++ cl.label)
          new Text(cl.label)
        }
        case None => {
          val rect = new Rectangle(10.0, 10.0)
          rect.setFill(Color.TRANSPARENT)
          rect
        }
      }
      // r.render(marker.dim)(marker.label)
    }

    pane.getChildren.add(label)

    val color : Color = 
      marker.label match {
        case Some(cl) => cl.color
        case None => Color.WHITE
      }

  }

}
