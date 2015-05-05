/**
  * ContextBuilder.scala - A JavaFX Control for Building a new Context
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx.opfibred

import scalafx.Includes._
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.geometry._

import javafx.scene.{layout => jfxsl}
import javafx.scene.{paint => jfxsp}

import opetopic._
import TypeDefs._
import syntax.complex._

import FibredLabel._

class ContextBuilder(val name: String, val baseContext: Context) extends StackPane {

  // This is pretty fucking ugly.
  def createContextViewer : FibredViewer = 
    new FibredViewer(
      baseContext.complex map(new ~~>[FibredLabel, FibredLblOpt] {
        def apply[N <: Nat](fl: FibredLabel[N]) : FibredLblOpt[N] = Some(fl)
      })
    )

  var lastColor : jfxsp.Color = jfxsp.Color.WHITE

  val viewer = createContextViewer
  val editor = new FibredEditor(this)

  viewer.setMaxSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)
  editor.setMaxSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)

  val grid = new GridPane {
    hgap = 10
    vgap = 10
    rowConstraints = List(
      new RowConstraints { percentHeight = 30 },
      new RowConstraints { percentHeight = 70 }
    )
    columnConstraints = List(
      new ColumnConstraints { hgrow = Priority.Always }
    )
  }

  val viewerStack = new StackPane {
    children = viewer
    style = "-fx-border-style: solid; -fx-border-width: 1px; -fx-border-color: grey"
  }

  val editorStack = new StackPane {
    children = editor
    style = "-fx-border-style: solid; -fx-border-width: 1px; -fx-border-color: grey"
  }

  grid.add(viewerStack, 0, 0)
  grid.add(editorStack, 0, 1)

  children += grid

  viewer.render
  editor.render  

  margin = Insets(10, 10, 10, 10)

}
