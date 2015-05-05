/**
  * FibredViewer.scala - A Viewer Implementation for OpFibred
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx.opfibred

import javafx.scene.Node
import javafx.scene.paint.Color

import opetopic._
import opetopic.fx._
import TypeDefs._
import syntax.complex._

import FibredLabel._

class FibredViewer(fc: FiniteComplex[FibredLblOpt]) extends FXComplexViewer[FibredLblOpt](fc) {

  type BoxType = FibredBox

  def createBox[N <: Nat](mk: FXComplexMarker[N]) : FibredBox = 
    new FibredBox {
      type Dim = N
      def marker = mk
    }

  abstract class FibredBox extends FXBox {

    val rdr = implicitly[FXRenderable[FibredLblOpt]]

    val label: Node = 
      rdr.render(marker.dim)(marker.label)

    pane.getChildren.add(label)

    val color: Color = 
      marker.label match {
        case None => Color.WHITE
        case Some(fl) => fl.color
      }

    override def doHoverStyle = {
      val hoverColor = 
        color.deriveColor(0, 1.0, .90, 1.0)
        //color.interpolate(Color.BLUE, 0.3)
      setBackground(genBg(hoverColor))
    }

    override def doUnhoverStyle = {
      setBackground(genBg(color))
    }

    override def doSelectedStyle = {
      val selectedColor = 
        color.deriveColor(0, 1.0, .80, 1.0)
        //color.interpolate(Color.BLUE, 0.5)
      setBackground(genBg(selectedColor))
    }

    override def doUnselectedStyle = {
      setBackground(genBg(color))
    }

  }

}

