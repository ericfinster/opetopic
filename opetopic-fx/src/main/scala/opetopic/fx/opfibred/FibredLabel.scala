/**
  * FibredLabel.scala - Labels for OpFibred
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx.opfibred

import javafx.scene.Node
import javafx.scene.text.Text
import javafx.scene.paint.Color

import opetopic._
import opetopic.fx._

abstract class FibredLabel[N <: Nat] {

  type ColoringDim <: Nat
  val coloringDim : ColoringDim

  val label: String
  val color: Color

  val address: Address[S[ColoringDim]]

}

object FibredLabel {

  type FibredLblOpt[N <: Nat] = Option[FibredLabel[N]]

  implicit object fibredLabelRenderable extends FXRenderable[FibredLabel] {
    def render[N <: Nat](n: N)(fl: FibredLabel[N]) : Node = 
      new Text(fl.label)
  }

  def apply[N <: Nat, D <: Nat](d: D)(l: String, c: Color, a: Address[S[D]]) = 
    new FibredLabel[N] {

      type ColoringDim = D
      val coloringDim = d

      val label = l
      val color = c
      val address = a

    }
}
