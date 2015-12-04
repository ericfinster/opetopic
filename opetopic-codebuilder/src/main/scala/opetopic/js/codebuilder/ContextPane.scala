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

class ContextPane { thisPane => 

  //============================================================================================
  // STATE
  //

  var activeCell : Option[Sigma[Cell]] = None

  val env = new EditorEnvironment {
    def registerCell[N <: Nat](cell: Cell[N]) : Unit = 
      thisPane.registerCell(cell)
  }

  val stack = new EditorStack(env)

  //============================================================================================
  // INITIALIZATION
  //

  def initialize : Unit = {

    jQuery(leftAccordion).accordion()
    jQuery(rightAccordion).accordion()

    stack.initialize
    stack.newInstance

  }

  //============================================================================================
  // BEHAVIORS
  //

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
      popup = environmentPopup,
      context = jQuery(uiElement),
      hoverable = "true",
      position = "right center",
      on = "click"
    ))

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
        div(cls := "active title")(i(cls := "dropdown icon"), "Goals"),
        div(cls := "active content")(p("Goals"))
      )
    ).render

  val pane = 
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

  val environmentPopup =
    div(id := "envPopup", cls := "ui vertical popup menu", style := "display: none")(
      a(cls := "item", onclick := { () => pasteToCursor })("Paste to Cursor"),
      a(cls := "item", onclick := { () => () })("Paste to New Editor"),
      a(cls := "item", onclick := { () => () })("Show Universal")
    ).render

  val uiElement = div(
    pane,
    environmentPopup
  ).render


}
