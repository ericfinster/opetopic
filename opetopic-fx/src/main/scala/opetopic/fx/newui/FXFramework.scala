/**
  * FXFramework.scala - FX Visual Framework Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx.newui

import javafx.scene.Node
import javafx.scene.text.{Text => JFXText}
import javafx.scene.shape.{Rectangle => JFXRectangle}

import opetopic.vf._

object FXFramework extends VisualFramework[Double] with PanelDeps[Double] {

  type ElementType = Node

  class FXRectangle(val x: Double, val y: Double, val width: Double, val height: Double) extends Rectangle {
    def render: Seq[Node] = Seq(new JFXRectangle(x, y, width, height))
  }

  class FXText(val text: String) extends Text {
    def render: Seq[Node] = Seq(new JFXText(text))
  }

  def rect(x: Double, y: Double, width: Double, height: Double) : Rectangle = 
    new FXRectangle(x, y, width, height)

}
