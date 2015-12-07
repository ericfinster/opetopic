/**
  * GoalPane.scala - A panel with a goal
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

abstract class GoalPane extends Pane { thisPane => 

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

    def registerParameter[N <: Nat](cell: Cell[N]) : Unit = ()
    def registerParameter[N <: Nat](prop : Property[N]) : Unit = ()

  }

  val stack = new EditorStack(thisPane)

  //============================================================================================
  // INITIALIZATION
  //

  def initialize : Unit = {

    jQuery(leftAccordion).accordion()
    jQuery(rightAccordion).accordion()

    stack.initialize
    stack.newInstance

    jQuery(goalPane).append(goal.render)

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

  def goal : HtmlTag
  val goalPane = div(cls := "ui raised segment").render

  val pane = 
    div(cls := "ui raised segment")(
      div(cls := "ui celled grid")(
        div(cls := "three wide column")(
          leftAccordion
        ),
        div(cls := "ten wide center aligned column")(
          goalPane,
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
