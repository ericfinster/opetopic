/**
  * DefinitionWorspaceUI.scala - UI Definitions for DefinitionWorkspace
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import org.scalajs.jquery._
import scalatags.JsDom.all._
import opetopic.js.JQuerySemanticUI._

trait DefinitionWorkspaceUI { self : DefinitionWorkspace => 

  def initUI: Unit = {

    jQuery(mainGrid).find(".ui.accordion").accordion()
    jQuery(mainGrid).find(".menu .item").tab()
    jQuery(mainGrid).find(".ui.checkbox").checkbox()

    jQuery(tabPane).keypress((e : JQueryEventObject) => {
      e.which match {
        case 101 => for { editor <- activeEditor } { editor.ce.extrudeSelection }
        case 100 => for { editor <- activeEditor } { editor.ce.extrudeDrop }
        case 112 => for { editor <- activeEditor } { editor.ce.sprout }
        case _ => ()
      }
    })

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

    jQuery(composeIdInput).on("input", () => {
      val compId = jQuery(composeIdInput).value().asInstanceOf[String]
      jQuery(composeFillInput).value(compId ++ "-fill")
      jQuery(composePropInput).value(compId ++ "-fill-isLeft")
    })

    jQuery(liftIdInput).on("input", () => {
      val liftId = jQuery(liftIdInput).value().asInstanceOf[String]
      jQuery(liftFillInput).value(liftId ++ "-fill")
      jQuery(liftLextInput).value(liftId ++ "-fill-isLeft")
      jQuery(liftRextInput).value(liftId ++ "-fill-isRight")
    })

  }

  val tabPane = div(cls := "ui middle attached nofocus segment", tabindex := 0).render
  val paginationMenu = div(cls := "ui pagination menu").render

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
        h3(cls := "ui dividing header")("Editor"),
        div(cls := "ui top attached menu")(
          a(cls := "item")("Shape", i(cls := "dropdown icon")),
          a(cls := "item")("Shell Force")
        ),
        tabPane,
        div(cls := "ui bottom attached segment")(
          div(cls := "ui grid")(
            div(cls := "fourteen wide column")(
              paginationMenu
            ),
            div(cls := "two wide right aligned column")(
              button(cls := "ui icon button", onclick := { () => newEditor })(i(cls := "add icon"))
            )
          )
        ),
        h3(cls := "ui dividing header")("Action"),
        div(cls := "ui grid")(
          div(cls := "four wide column")(
            div(cls := "ui vertical fluid pointing menu")(
              a(cls := "active item", "data-tab".attr := "assume-tab")("Assume"),
              a(cls := "item", "data-tab".attr := "compose-tab")("Compose"),
              a(cls := "item", "data-tab".attr := "lift-tab")("Lift")
            )
          ),
          div(cls := "twelve wide stretched column")(
            div(cls := "ui segment")(
              div(cls := "ui active tab", "data-tab".attr := "assume-tab")(assumeForm),
              div(cls := "ui tab", "data-tab".attr := "compose-tab")(composeForm),
              div(cls := "ui tab", "data-tab".attr := "lift-tab")(liftForm)
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
