/**
  * EditorStack.scala - A Collection of Editors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic.js._
import JQuerySemanticUI._

class EditorStack(pane: Pane) {

  //============================================================================================
  // STATE
  //

  var activeInstance : Option[EditorInstance] = None
  var instanceCount : Int = 0
  var onSidebarShown : () => Unit = () => ()

  def env = pane.env

  //============================================================================================
  // INITIALIZATION
  //

  def initialize: Unit = {

    jQuery(uiElement).find(".ui.dropdown").dropdown(lit(on = "hover"))

    jQuery(formSidebar).sidebar(lit(
      context = jQuery(tabContainer),
      dimPage = false,
      closable = false,
      onShow = { () => onSidebarShown() }
    ))

  }

  //============================================================================================
  // BEHAVIORS
  //

  def newInstance: Unit = {

    val instance = new EditorInstance(env)
    instanceCount += 1

    val icStr = instanceCount.toString
    val tabName = "tab-" ++ icStr

    val tab = div(cls := "ui tab", "data-tab".attr := tabName)(
      instance.editor.element.uiElement
    ).render

    val label = 
      a(cls := "ui grey circular label", 
        onclick := { () => { jQuery(tab).tab("change tab", tabName) ; activeInstance = Some(instance) } }
      )(icStr).render

    jQuery(tabs).append(tab)
    jQuery(tabLabels).append(label)
    jQuery(tab).tab("change tab", tabName)

    activeInstance = Some(instance)

    println("Created new instance ...")

  }

  def onExportToSVG: Unit = 
    for {
      i <- activeInstance
      svgString <- i.selectionToSvg
    } {

      jQuery(".ui.modal.svgexport").find("#exportlink").
        attr(lit(href = "data:text/plain;charset=utf-8," ++
          js.URIUtils.encodeURIComponent(svgString)))
      jQuery(".ui.modal.svgexport").modal("show")

    }

  def onAssumeVariable: Unit = {

    val idInput = input(`type` := "text", placeholder := "Identifier").render
    val isLeftExt = input(`type` := "checkbox", tabindex := 0, cls := "hidden").render
    // val isRightExt = input(`type` := "checkbox", tabindex := 0, cls := "hidden")

    val varForm =
      form(cls := "ui form")(
        div(cls := "inline fields")(
          div(cls := "field")(
            label("Assume Variable:"),
            idInput
          ),
          div(cls := "field")(
            div(cls := "ui checkbox")(
              isLeftExt,
              label("Left Extension")
            )
          ),
          // div(cls := "field")(
          //   div(cls := "ui checkbox")(
          //     isRightExt,
          //     label("Right Extension")
          //   )
          // ),
          button(cls := "ui button", `type` := "submit")("Ok")
        )
      ).render

    jQuery(formSidebar).empty().append(
      div(cls := "ui basic segment")(varForm).render
    )

    jQuery(varForm).find(".checkbox").checkbox()

    onSidebarShown = { () => jQuery(idInput).focus() }

    jQuery(varForm).on("submit", (e : JQueryEventObject) => { 
      e.preventDefault 
      pane.hotkeysEnabled = true
      jQuery(formSidebar).sidebar("hide")
      pane.focus
      val id = jQuery(idInput).value().asInstanceOf[String]
      val lex = jQuery(isLeftExt).prop("checked").asInstanceOf[Boolean]
      for { i <- activeInstance } { i.assumeVariable(id, lex) }
    })

    pane.hotkeysEnabled = false
    jQuery(formSidebar).sidebar("show")

  }

  def onComposeDiagram : Unit = {

    val idInput = input(`type` := "text", placeholder := "Identifier").render

    val compForm =
      form(cls := "ui form")(
        div(cls := "inline fields")(
          div(cls := "field")(
            label("Compose Diagram:"),
            idInput
          ),
          button(cls := "ui button", `type` := "submit")("Ok")
        )
      ).render

    jQuery(formSidebar).empty().append(
      div(cls := "ui basic segment")(compForm).render
    )

    onSidebarShown = { () => jQuery(idInput).focus() }

    jQuery(compForm).on("submit", (e : JQueryEventObject) => { 
      e.preventDefault 
      pane.hotkeysEnabled = true
      jQuery(formSidebar).sidebar("hide")
      pane.focus
      val id = jQuery(idInput).value().toString
      for { i <- activeInstance } { i.composeDiagram(id) }
    })

    pane.hotkeysEnabled = false
    jQuery(formSidebar).sidebar("show")

  }

  def onLeftLift : Unit = {

    val idInput = input(`type` := "text", placeholder := "Identifier").render

    val liftForm =
      form(cls := "ui form")(
        div(cls := "inline fields")(
          div(cls := "field")(
            label("Lift Cell:"),
            idInput
          ),
          button(cls := "ui button", `type` := "submit")("Ok")
        )
      ).render

    jQuery(formSidebar).empty().append(
      div(cls := "ui basic segment")(liftForm).render
    )

    onSidebarShown = { () => jQuery(idInput).focus() }

    jQuery(liftForm).on("submit", (e : JQueryEventObject) => { 
      e.preventDefault 
      pane.hotkeysEnabled = true
      jQuery(formSidebar).sidebar("hide")
      pane.focus
      val id = jQuery(idInput).value().toString
      for { i <- activeInstance } { i.leftLift(id) }
    })

    pane.hotkeysEnabled = false
    jQuery(formSidebar).sidebar("show")

  }

  def onRightLift : Unit = {

    val idInput = input(`type` := "text", placeholder := "Identifier").render

    val liftForm =
      form(cls := "ui form")(
        div(cls := "inline fields")(
          div(cls := "field")(
            label("Lift Cell:"),
            idInput
          ),
          button(cls := "ui button", `type` := "submit")("Ok")
        )
      ).render

    jQuery(formSidebar).empty().append(
      div(cls := "ui basic segment")(liftForm).render
    )

    onSidebarShown = { () => jQuery(idInput).focus() }

    jQuery(liftForm).on("submit", (e : JQueryEventObject) => { 
      e.preventDefault 
      pane.hotkeysEnabled = true
      jQuery(formSidebar).sidebar("hide")
      pane.focus
      val id = jQuery(idInput).value().toString
      for { i <- activeInstance } { i.rightLift(id) }
    })

    pane.hotkeysEnabled = false
    jQuery(formSidebar).sidebar("show")

  }

  //============================================================================================
  // UI COMPONENTS
  //

  val tabs = div(cls := "ui basic segment", style := "min-height: 310px").render
  val formSidebar = div(cls := "ui bottom sidebar", style := "z-index: 10 ; background-color: white").render

  val tabContainer = div(cls := "ui attached pushable segment", style := "overflow: hidden")(
    formSidebar,
    div(cls := "pusher")(
      tabs
    )
  ).render

  val tabLabels = 
    div(cls := "ui center aligned bottom attached segment")(
      a(cls := "ui grey circular label", onclick := { () => newInstance })("+")
    ).render

  val uiElement = 
    div(
      div(cls := "ui top attached menu")(
        div(cls := "ui dropdown item")(
          "Shape",
          i(cls := "dropdown icon"),
          div(cls := "menu")(
            a(cls := "item")("Extrude"),
            a(cls := "item")("Drop"),
            a(cls := "item")("Precompose")
          )
        ),
        div(cls := "ui dropdown item")(
          "Term",
          i(cls := "dropdown icon"),
          div(cls := "menu")(
            a(cls := "item")("Assume Variable"),
            a(cls := "item")("Compose Diagram")
          )
        )
      ),
      tabContainer,
      tabLabels
    ).render

}
