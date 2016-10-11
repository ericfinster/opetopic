/**
  * DefinitionWorspaceUI.scala - UI Definitions for DefinitionWorkspace
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic.js._
import opetopic.mtl._
import JsDomFramework._
import JQuerySemanticUI._

import Prover.runExcept
import Prover.parseExpr

abstract class DefinitionWorkspaceUI extends JsStableEditor[Marker] with Workspace {
  self : DefinitionWorkspace =>

  override def handleKeyEvent(ev: JQueryEventObject): Unit = {
    ev.which match {
      case 112 => jQuery("#paste-expr-modal").modal("show")
      case _ => super.handleKeyEvent(ev)
    }
  }

  def initUI: Unit = {

    println("Initializing definition workspace ...")

    jQuery(tocPane).accordion(lit(
      exclusive = false
    ))

    jQuery(baseBar).find(".item").tab()
    jQuery(baseBar).find(".ui.checkbox").checkbox()

    jQuery(assumeForm).on("submit",
      (e : JQueryEventObject) => {
        e.preventDefault
        runExcept(onAssume)
      })

    jQuery(composeForm).on("submit",
      (e : JQueryEventObject) => {
        e.preventDefault
        runExcept(onCompose)
      })

    jQuery(tgtLiftBtn).on("click", 
      (e : JQueryEventObject) => {
        e.preventDefault
        runExcept(onTargetLift)
      })

    jQuery(srcLiftBtn).on("click",
      (e : JQueryEventObject) => {
        e.preventDefault
        runExcept(onSourceLift)
      })


    jQuery("#paste-expr-modal").modal(lit(
      onApprove = () => {

        val exprStr = jQuery("#paste-expr-input").value().asInstanceOf[String]
        println("Trying to parse expression: " + exprStr)

        Prover.parseExpr(exprStr) match {
          case Xor.Left(s) => println("Error during parse: " + s)
          case Xor.Right(e) => runExcept(onPaste(e, exprStr))
        }

      }
    ))

    jQuery(tgtClosureBtn).on("click",
      (e : JQueryEventObject) => {
      e.preventDefault
      runExcept(onTargetClosure)
    })

    jQuery(composeIdInput).on("input", () => {
      val compId = jQuery(composeIdInput).value().asInstanceOf[String]
      jQuery(composeFillInput).value(compId ++ "-fill")
      jQuery(composePropInput).value(compId ++ "-tgt")
    })

    jQuery(liftIdInput).on("input", () => {
      val liftId = jQuery(liftIdInput).value().asInstanceOf[String]
      jQuery(liftFillInput).value(liftId ++ "-fill")
      jQuery(liftTgtInput).value(liftId ++ "-tgt")
      jQuery(liftSrcInput).value(liftId ++ "-src")
    })

    initialize

  }

  //============================================================================================
  // ASSUME FORM
  //

  val assumeIdInput = input(`type` := "text", placeholder := "Identifier").render
  val assumeTgtExtCheckbox = input(`type` := "checkbox", tabindex := 0, cls := "hidden").render
  val assumeSrcExtCheckbox = input(`type` := "checkbox", tabindex := 0, cls := "hidden").render

  val assumeForm =
    form(cls := "ui form")(
      div(cls := "ui grid")(
        div(cls := "eight wide column")(
          div(cls := "field")(label("Assume Variable:"), assumeIdInput),
          div(cls := "field")(
            div(cls := "ui checkbox")(
              assumeTgtExtCheckbox,
              label("Target Extension")
            )
          ),
          div(cls := "field")(
            div(cls := "ui checkbox")(
              assumeSrcExtCheckbox,
              label("Source Extension")
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
  val liftTgtInput = input(`type` := "text", placeholder := "Identifier").render
  val liftSrcInput = input(`type` := "text", placeholder := "Identifier").render

  val tgtLiftBtn = button(`type` := "button", cls := "ui green button")("Target Lift").render
  val srcLiftBtn = button(`type` := "button", cls := "ui green button")("Source Lift").render

  val liftForm = 
    form(cls := "ui form")(
      div(cls := "ui grid")(
        div(cls := "eight wide column")(
          div(cls := "field")(label("Lift:"), liftIdInput),
          div(cls := "field")(label("Filler:"), liftFillInput)
        ),
        div(cls := "eight wide column")(
          div(cls := "field")(label("Target Property:"), liftTgtInput),
          div(cls := "field")(label("Source Property:"), liftSrcInput),
          tgtLiftBtn, srcLiftBtn
        )
      )
    ).render

  //============================================================================================
  // TARGET CLOSURE
  //

  val tgtClosureBtn = 
    button(cls := "ui button", id := "tgt-closure-btn")("Target Closure").render

  //============================================================================================
  // LISTS
  //

  val contextList = div(cls := "menu").render
  val environmentList = div(cls := "menu").render
  val cellList = div(cls := "menu").render
  val propertyList = div(cls := "menu").render

  //============================================================================================
  // MAIN LAYOUT
  //

  val tocPane =
    div(cls := "opetopic ui vertical fluid accordion auxillary menu")(
      div(cls := "item")(
        div(cls := "header active title")(i(cls := "dropdown icon"), "Context"),
        div(cls := "active content")(contextList)
      ),
      div(cls := "item")(
        div(cls := "header active title")(i(cls := "dropdown icon"), "Environment"),
        div(cls := "active content")(environmentList)
      ),
      div(cls := "item")(
        div(cls := "header active title")(i(cls := "dropdown icon"), "Cells"),
        div(cls := "active content")(cellList)
      ),
      div(cls := "item")(
        div(cls := "header active title")(i(cls := "dropdown icon"), "Properties"),
        div(cls := "active content")(propertyList)
      )
    ).render

  val articlePane = div(uiElement).render

  val propsPane = div().render
  
  val baseBar =
    div(cls := "ui basic segment")(
      div(cls := "ui grid")(
        div(cls := "four wide column")(
          div(cls := "ui secondary pointing menu")(
            a(cls := "active item")("Properties")
          ),
          div(cls := "ui segment", style := "min-height: 300px;")(
            propsPane
          )
        ),

        div(cls := "twelve wide column")(
          div(cls := "ui secondary pointing menu")(
            a(cls := "active item", attr("data-tab") := "assume-tab")("Assume"),
            a(cls := "item", attr("data-tab") := "compose-tab")("Compose"),
            a(cls := "item", attr("data-tab") := "lift-tab")("Lift"),
            a(cls := "item", attr("data-tab") := "closure-tab")("Closure")
          ),

          div(cls := "ui segment", style := "min-height: 300px;")(
            div(cls := "ui active tab", attr("data-tab") := "assume-tab")(assumeForm),
            div(cls := "ui tab", attr("data-tab") := "compose-tab")(composeForm),
            div(cls := "ui tab", attr("data-tab") := "lift-tab")(liftForm),
            div(cls := "ui tab", attr("data-tab") := "closure-tab")(tgtClosureBtn)
          )
          
        )
      )
    ).render

}
