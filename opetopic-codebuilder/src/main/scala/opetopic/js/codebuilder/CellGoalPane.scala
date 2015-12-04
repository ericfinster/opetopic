/**
  * CellGoalPane.scala - A Goal Pane for defining cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.js._
import opetopic.tt._
import syntax.all._

import JQuerySemanticUI._
import JsDomFramework._
import Cell.ActiveInstance._
import Cell._

class CellGoalPane[N <: Nat](frm: Complex[Cell, N]) extends GoalPane {

  implicit val emptyBnds = defaultGalleryConfig.spacerBounds

  val optComplex = frm.map[OptCell](toOptionMap[Cell]) >> Dot(None, frm.length)
  val goalComplex = ActiveGallery[OptCell](optComplex)

  val goal = div(
    h2(cls := "ui dividing header")("Goal"),
    goalComplex.element.uiElement
  )


}
