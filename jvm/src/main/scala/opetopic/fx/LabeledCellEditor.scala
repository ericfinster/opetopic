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

  import upickle._

  implicit def labelWriter[N <: Nat] : IndexedWriter[LabelOpt] = 
    new IndexedWriter[LabelOpt] {
      def writer[N <: Nat] : Writer[LabelOpt[N]] =  
        Writer[LabelOpt[N]] {
          case None => Js.Arr()
          case Some(ColoredLabel(lbl, cl)) => 
            Js.Arr(Js.Obj(("label", Js.Str(lbl)), ("color", Js.Str(cl.toString))))
        }
    }

  implicit def labelReader[N <: Nat] : IndexedReader[LabelOpt] = 
    new IndexedReader[LabelOpt] {
      def reader[N <: Nat] : Reader[LabelOpt[N]] = 
        Reader[LabelOpt[N]] {
          case Js.Arr(els @ _*) => {
            for {
              Js.Obj(("label", Js.Str(lbl)), ("color", Js.Str(cl))) <- els.headOption
            } yield ColoredLabel(lbl, Color.web(cl))
          }
        }
    }

}

class LabeledCellEditor extends FXCardinalEditor[ColoredLabel] {

  def this(cmplx: FiniteComplex[ColoredLabel.LabelOpt]) = {
    this()
    val labelCardinal = Cardinal.complexToCardinal(cmplx.value)._1
    val newState = initializeEditor(labelCardinal)
    editorState = newState
  }

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

    val label : Node = 
      r.render(marker.dim)(marker.label)

    pane.getChildren.add(label)

    val color : Color = 
      marker.label match {
        case Some(cl) => cl.color
        case None => Color.WHITE
      }

  }

}
