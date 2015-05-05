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
import TypeDefs._

case class FibredLabel[N <: Nat](
  val label: String,
  val color: Color,
  val address: Sigma[Address]
)

object FibredLabel {

  type FibredLblOpt[N <: Nat] = Option[FibredLabel[N]]

  implicit object fibredLabelRenderable extends FXRenderable[FibredLabel] {
    def render[N <: Nat](n: N)(fl: FibredLabel[N]) : Node = 
      new Text(fl.label)
  }

}
