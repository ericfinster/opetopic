/**
  * DefinitionWorspaceUI.scala - UI Definitions for DefinitionWorkspace
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic.js._
import JsDomFramework._
import JQuerySemanticUI._
import Marker.ActiveInstance._

import Prover.runAction

abstract class DefinitionWorkspaceUI extends JsCardinalEditor[Marker] { self : DefinitionWorkspace => 

  implicit val vf: VisualizableFamily[Marker] = 
    cellMarkerVisFam

  def initUI: Unit = {

    jQuery(mainGrid).find(".ui.accordion").accordion()
    jQuery(mainGrid).find(".menu .item").tab()
    jQuery(mainGrid).find(".ui.checkbox").checkbox()

    jQuery(assumeForm).on("submit",
      (e : JQueryEventObject) => {
        e.preventDefault
        runAction(onAssume)
      })

    jQuery(composeForm).on("submit",
      (e : JQueryEventObject) => {
        e.preventDefault
        runAction(onCompose)
      })

    jQuery(importForm).on("submit",
      (e : JQueryEventObject) => {
        e.preventDefault
        runAction(onImport)
      })

    jQuery(leftLiftBtn).on("click", 
      (e : JQueryEventObject) => {
        e.preventDefault
        runAction(onLeftLift)
      })

    jQuery(rightLiftBtn).on("click",
      (e : JQueryEventObject) => {
        e.preventDefault
        runAction(onRightLift)
      })

    jQuery(shellForceBtn).on("click", 
      (e : JQueryEventObject) => {
      e.preventDefault
      runAction(onShellForce)
    })

    jQuery(composeIdInput).on("input", () => {
      val compId = jQuery(composeIdInput).value().asInstanceOf[String]
      jQuery(composeFillInput).value(compId ++ "Fill")
      jQuery(composePropInput).value(compId ++ "FillIsLeft")
    })

    jQuery(liftIdInput).on("input", () => {
      val liftId = jQuery(liftIdInput).value().asInstanceOf[String]
      jQuery(liftFillInput).value(liftId ++ "Fill")
      jQuery(liftLextInput).value(liftId ++ "FillIsLeft")
      jQuery(liftRextInput).value(liftId ++ "FillIsRight")
    })

    initialize

  }

  //============================================================================================
  // ASSUME FORM
  //

  val assumeIdInput = input(`type` := "text", placeholder := "Identifier").render
  val assumeLextCheckbox = input(`type` := "checkbox", tabindex := 0, cls := "hidden").render
  val assumeRextCheckbox = input(`type` := "checkbox", tabindex := 0, cls := "hidden").render

  val assumeForm = 
    form(cls := "ui form")(
      div(cls := "ui grid")(
        div(cls := "eight wide column")(
          div(cls := "field")(label("Assume Variable:"), assumeIdInput),
          div(cls := "field")(
            div(cls := "ui checkbox")(
              assumeLextCheckbox,
              label("Left Extension")
            )
          ),
          div(cls := "field")(
            div(cls := "ui checkbox")(
              assumeRextCheckbox,
              label("Right Extension")
            )
          )
        ),
        div(cls := "eight wide column")(
        )
      )
    ).render

  //============================================================================================
  // COMPOSE FORM
  //

  val composeIdInput = input(`type` := "text", placeholder := "Identifier").render
  val composeFillInput = input(`type` := "text", placeholder := "Identifier").render
  val composePropInput = input(`type` := "text", placeholder := "Identifier").render

  val composeForm = 
    form(cls := "ui form")(
      div(cls := "ui grid")(
        div(cls := "eight wide column")(
          div(cls := "field")(label("Composite:"), composeIdInput),
          div(cls := "field")(label("Filler:"), composeFillInput)
        ),
        div(cls := "eight wide column")(
          div(cls := "field")(label("Property:"), composePropInput),
          button(`type` := "submit", cls := "ui green button")("Go!")
        )
      )
    ).render

  //============================================================================================
  // LIFT FORM
  //

  val liftIdInput = input(`type` := "text", placeholder := "Identifier").render
  val liftFillInput = input(`type` := "text", placeholder := "Identifier").render
  val liftLextInput = input(`type` := "text", placeholder := "Identifier").render
  val liftRextInput = input(`type` := "text", placeholder := "Identifier").render

  val leftLiftBtn = button(`type` := "button", cls := "ui green button")("Lift Left").render
  val rightLiftBtn = button(`type` := "button", cls := "ui green button")("Lift Right").render

  val liftForm = 
    form(cls := "ui form")(
      div(cls := "ui grid")(
        div(cls := "eight wide column")(
          div(cls := "field")(label("Lift:"), liftIdInput),
          div(cls := "field")(label("Filler:"), liftFillInput)
        ),
        div(cls := "eight wide column")(
          div(cls := "field")(label("Left Property:"), liftLextInput),
          div(cls := "field")(label("Right Property:"), liftRextInput),
          leftLiftBtn, rightLiftBtn
        )
      )
    ).render

  //============================================================================================
  // IMPORT FORM
  //

  val importIdInput = input(`type` := "text", placeholder := "Identifier").render
  val importExprInput = input(`type` := "text", placeholder := "Expression").render

  val importForm = 
    form(cls := "ui form")(
      div(cls := "field")(label("Display Name: "), importIdInput),
      div(cls := "field")(label("Expression: "), importExprInput),
      button(`type` := "submit", cls := "ui green button")("Do It!")
    ).render

  //============================================================================================
  // SHELL FORCE
  //

  val shellForceBtn = 
    button(cls := "ui button", id := "force-btn")("Shell Force").render

  //============================================================================================
  // LISTS
  //

  val contextList = div(cls := "ui fluid styled accordion").render
  val environmentList = div(cls := "ui fluid styled accordion").render
  val cellList = div(cls := "ui fluid styled accordion").render
  val propertyList = div(cls := "ui fluid styled accordion").render

  //============================================================================================
  // MAIN LAYOUT
  //

  val mainGrid = 
    div(cls := "ui grid")(
      div(cls := "three wide column")(
        div(cls := "ui secondary pointing menu")(
          a(cls := "active item", "data-tab".attr := "context-tab")(h3("Context")),
          a(cls := "item", "data-tab".attr := "environment-tab")(h3("Environment"))
        ),
        div(cls := "ui active tab", "data-tab".attr := "context-tab")(contextList),
        div(cls := "ui tab", "data-tab".attr := "environment-tab")(environmentList)
      ),
      div(cls := "ten wide column")(
        h3(cls := "ui dividing header")("Editor - " + module.name),
        uiElement,
        h3(cls := "ui dividing header")("Action"),
        div(cls := "ui grid")(
          div(cls := "four wide column")(
            div(cls := "ui vertical fluid pointing menu")(
              a(cls := "active item", "data-tab".attr := "assume-tab")("Assume"),
              a(cls := "item", "data-tab".attr := "compose-tab")("Compose"),
              a(cls := "item", "data-tab".attr := "lift-tab")("Lift"),
              a(cls := "item", "data-tab".attr := "import-tab")("Import"),
              a(cls := "item", "data-tab".attr := "force-tab")("Force")
            )
          ),
          div(cls := "twelve wide stretched column")(
            div(cls := "ui segment")(
              div(cls := "ui active tab", "data-tab".attr := "assume-tab")(assumeForm),
              div(cls := "ui tab", "data-tab".attr := "compose-tab")(composeForm),
              div(cls := "ui tab", "data-tab".attr := "lift-tab")(liftForm),
              div(cls := "ui tab", "data-tab".attr := "import-tab")(importForm),
              div(cls := "ui tab", "data-tab".attr := "force-tab")(
                shellForceBtn
              )
            )
          )
        )
      ),
      div(cls := "three wide column")(
        div(cls := "ui secondary pointing menu")(
          a(cls := "active item", "data-tab".attr := "cells-tab")(h3("Cells")),
          a(cls := "item", "data-tab".attr := "props-tab")(h3("Properties"))
        ),
        div(cls := "ui active tab", "data-tab".attr := "cells-tab")(cellList),
        div(cls := "ui tab", "data-tab".attr := "props-tab")(propertyList)
      )
    ).render

}
