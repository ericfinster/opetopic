/**
  * ContextPane.scala - Basic Pane For Building Up a Context
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

import JQuerySemanticUI._

class ContextPane extends Pane { thisPane => 

  //============================================================================================
  // STATE
  //

  var activeCell : Option[Sigma[Cell]] = None
  var activeProperty : Option[Sigma[Property]] = None

  val env = new EditorEnvironment {

    def registerCell[N <: Nat](cell: Cell[N]) : Unit = 
      thisPane.registerCell(cell)

    def registerProperty[N <: Nat](prop: Property[N]) : Unit = 
      thisPane.registerProperty(prop)

  }

  val stack = new EditorStack(thisPane)

  //============================================================================================
  // INITIALIZATION
  //

  def initialize : Unit = {

    jQuery(leftAccordion).accordion()
    jQuery(rightAccordion).accordion()

    jQuery(uiElement).keypress((e : JQueryEventObject) => {
      if (hotkeysEnabled) {
        e.which match {
          case 97 => stack.onAssumeVariable 
          case 102 => stack.onComposeDiagram
          case 101 => for { i <- stack.activeInstance } { i.editor.extrudeSelection }
          case 100 => for { i <- stack.activeInstance } { i.editor.extrudeDrop }
          case 112 => for { i <- stack.activeInstance } { i.editor.sprout }
          case 103 => newCellGoal
          case 108 => stack.onLeftLift
          case 114 => stack.onRightLift
          case 118 => stack.onExportToSVG
          case _ => ()
        }
      }
    })

    stack.initialize
    stack.newInstance

  }

  //============================================================================================
  // BEHAVIORS
  //

  def focus: Unit = jQuery(uiElement).focus()

  def pasteToCursor: Unit = 
    for {
      cell <- activeCell
      i <- stack.activeInstance
    }{ i.doPaste(cell.n)(cell.value) }

  def newCellGoal: Unit = 
    for {
      i <- stack.activeInstance
      frm <- i.selectionFrame
    } {

      val pane = new CellGoalPane(frm.value)
      jQuery("#panes").append(pane.uiElement)
      pane.initialize

    }

  def registerCell[N <: Nat](cell: Cell[N]) : Unit = {

    val item =
      div(
        cls := "item",
        onclick := { () => activeCell = Some(Sigma(cell.dim)(cell)) }
      )(
        div(cls := "content", style := "margin-left: 10px")(cell.id)
      ).render


    jQuery(cellList).append(item)

    jQuery(item).popup(lit(
      movePopup = false,
      popup = cellPopup,
      context = jQuery(uiElement),
      hoverable = "true",
      position = "right center",
      on = "click"
    ))

  }

  def registerProperty[N <: Nat](prop: Property[N]) : Unit = {

    val item =
      div(
        cls := "item",
        onclick := { () => activeProperty = Some(Sigma(prop.dim)(prop)) }
      )(
        div(cls := "content", style := "margin-left: 10px")(prop.id)
      ).render


    jQuery(propertyList).append(item)

  }

  //============================================================================================
  // UI COMPONENTS
  //

  val cellList = 
    div(cls := "ui large selection list").render

  val propertyList = 
    div(cls := "ui large selection list").render

  val leftAccordion = 
    div(cls := "ui fluid vertical accordion menu")(
      div(cls := "item")(
        div(cls := "active title")(i(cls := "dropdown icon"), "Cells"),
        div(cls := "active content")(cellList)
      ),
      div(cls := "item")(
        div(cls := "title")(i(cls := "dropdown icon"), "Properties"),
        div(cls := "content")(propertyList)
      )
    ).render

  val rightAccordion = 
    div(cls := "ui fluid vertical accordion menu")(
      div(cls := "item")(
        div(cls := "active title")(i(cls := "dropdown icon"), "Context")
      )
    ).render

  val pane : HtmlTag = 
    div(cls := "ui raised segment")(
      div(cls := "ui celled grid")(
        div(cls := "three wide column")(
          leftAccordion
        ),
        div(cls := "ten wide center aligned column")(
          stack.uiElement
        ),
        div(cls := "three wide column")(
          rightAccordion
        )
      )
    )

  val cellPopup =
    div(id := "envPopup", cls := "ui vertical popup menu", style := "display: none")(
      a(cls := "item", onclick := { () => pasteToCursor })("Paste to Cursor"),
      a(cls := "item", onclick := { () => () })("Paste to New Editor"),
      a(cls := "item", onclick := { () => () })("Show Universal")
    ).render

  val uiElement = 
    div(tabindex := 0)(
      pane,
      cellPopup
    ).render

}
