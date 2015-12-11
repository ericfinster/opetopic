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

import scalaz.-\/
import scalaz.\/-

import opetopic._
import opetopic.js._
import JQuerySemanticUI._

class EditorStack(pane: Pane with Registry) {

  //============================================================================================
  // STATE
  //

  var activeInstance : Option[EditorInstance] = None
  var instanceCount : Int = 0
  var rightExtAddress : Option[Sigma[Address]] = None

  def env = pane.env

  //============================================================================================
  // INITIALIZATION
  //

  def initialize: Unit = {

    jQuery(uiElement).find(".ui.dropdown").dropdown(lit(on = "hover"))

    jQuery(assumeItem).popup(lit(
      inline = true,
      position = "bottom left",
      hoverable = "true",
      on = "click",
      onShow = { () => onAssumePopupShow },
      onVisible = { () => onAssumePopupVisible },
      onHidden = { () => onAssumePopupHidden }
    ))

    jQuery(composeItem).popup(lit(
      inline = true,
      position = "bottom left",
      hoverable = "true",
      on = "click",
      onVisible = { () => onComposePopupVisible },
      onHidden = { () => onComposePopupHidden }
    ))

    jQuery(lexItem).popup(lit(
      inline = true,
      position = "bottom left",
      hoverable = "true",
      on = "click",
      onVisible = { () => onLexPopupVisible },
      onHidden = { () => onLexPopupHidden }
    ))

    jQuery(rexItem).popup(lit(
      inline = true,
      position = "bottom left",
      hoverable = "true",
      on = "click",
      onVisible = { () => onRexPopupVisible },
      onHidden = { () => onRexPopupHidden }
    ))

    jQuery(assumePopup).find(".ui.checkbox").checkbox()

    jQuery(assumePopup).find(".ui.form").on("submit", 
      (e : JQueryEventObject) => {
        e.preventDefault
        onAssumePopupSubmit
      })

    jQuery(composePopup).find(".ui.form").on("submit", 
      (e : JQueryEventObject) => {
        e.preventDefault
        onComposePopupSubmit
      })

    jQuery(lexPopup).find(".ui.form").on("submit", 
      (e : JQueryEventObject) => {
        e.preventDefault
        onLexPopupSubmit
      })

    jQuery(rexPopup).find(".ui.form").on("submit", 
      (e : JQueryEventObject) => {
        e.preventDefault
        onRexPopupSubmit
      })

  }

  //============================================================================================
  // BEHAVIORS
  //

  def newInstance: Unit = {

    val instance = new EditorInstance(pane, env)
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

  def run[A](m: EditorM[A]) : Unit = {
    m match {
      case -\/(msg) => println(msg)
      case \/-(a) => ()
    }
  }

  //============================================================================================
  // ASSUME VARIABLE EVENT
  //

  def onAssumeVariable: Unit = 
    jQuery(assumeItem).popup("show")

  def onAssumePopupShow : Unit = {
    run(
      for {
        i <- attempt(activeInstance, "No active instance")
        _ <- i.withSelection(new i.BoxAction[Unit] {

          import Cell.ActiveInstance._
          import JsDomFramework._
          import syntax.complex._

          def objectAction(box: i.EditorBox[_0]) : EditorM[Unit] = {
            jQuery(frameDisplay).empty()
            editorSucceed(())
          }

          def cellAction[Q <: Nat](q: Q)(box : i.EditorBox[S[Q]]) : EditorM[Unit] = {
            for {
              cellCmplx <- new i.SuccBoxOps(box).cellComplex
            } yield {
              val panel = ActivePanel(cellCmplx.head)

              panel.onBoxClicked = { (box: SimpleActiveCellBox[Cell[Q], Q]) => {
                if (box.isExternal) {
                  rightExtAddress = Some(Sigma[Address, Q](q)(box.address.head))
                }
              }}

              jQuery(frameDisplay).empty().append(panel.element.uiElement)
            }
          }
        })
      } yield ()
    )
  }

  def onAssumePopupVisible : Unit = {

    pane.hotkeysEnabled = false
    jQuery(varIdInput).focus()

  }

  def onAssumePopupHidden : Unit = {

    jQuery(varIdInput).value("")
    pane.hotkeysEnabled = true
    pane.focus

  }

  def onAssumePopupSubmit : Unit = {

    val id = jQuery(varIdInput).value().asInstanceOf[String]
    val lex = jQuery(isLexCheckbox).prop("checked").asInstanceOf[Boolean]
    val rex = jQuery(isRexCheckbox).prop("checked").asInstanceOf[Boolean]

    jQuery(assumeItem).popup("hide")

    run(
      for {
        i <- attempt(activeInstance, "No active instance")
        _ <- i.withSelection(new i.BoxAction[Unit] {

          def objectAction(box: i.EditorBox[_0]) : EditorM[Unit] = 
            i.assumeObject(box, id)

          def cellAction[Q <: Nat](q: Q)(box : i.EditorBox[S[Q]]) : EditorM[Unit] = {

            import TypeLemmas._

            for {
              rexAddrOpt <- (
                if (rex) {
                  for {
                    addrSig <- attempt(rightExtAddress, "Right extension selected, but no address given.")
                    ev <- attempt(matchNatPair(addrSig.n, q), "Address has wrong dimension")
                    addr = rewriteNatIn[Address, addrSig.N, Q](ev)(addrSig.value)
                  } yield Some(addr)
                } else editorSucceed(None)
              )
              _ <- i.assumeCell(q)(box, id, lex, rexAddrOpt)
            } yield ()
          }
        })
      } yield ()
    )

  }

  //============================================================================================
  // COMPOSE EVENT
  //

  def onComposeDiagram : Unit = 
    jQuery(composeItem).popup("show")


  def onComposePopupVisible : Unit = {

    pane.hotkeysEnabled = false
    jQuery(compIdInput).focus()

  }

  def onComposePopupHidden : Unit = {

    jQuery(compIdInput).value("")
    pane.hotkeysEnabled = true
    pane.focus

  }

  def onComposePopupSubmit : Unit = {

    val id = jQuery(compIdInput).value().asInstanceOf[String]
    jQuery(composeItem).popup("hide")

    run(
      for {
        i <- attempt(activeInstance, "No active instance")
        _ <- i.composeDiagram(id)
      } yield ()
    )
  }

  //============================================================================================
  // LEFT EXTEND EVENT
  //

  def onLeftLift : Unit = 
    jQuery(lexItem).popup("show")

  def onLexPopupVisible : Unit = {

    pane.hotkeysEnabled = false
    jQuery(lexIdInput).focus()

  }

  def onLexPopupHidden : Unit = {

    jQuery(lexIdInput).value("")
    pane.hotkeysEnabled = true
    pane.focus

  }

  def onLexPopupSubmit : Unit = {

    val id = jQuery(lexIdInput).value().asInstanceOf[String]
    jQuery(lexItem).popup("hide")

    run(
      for {
        i <- attempt(activeInstance, "No active instance")
        _ <- i.leftLift(id)
      } yield ()
    )

  }

  //============================================================================================
  // RIGHT EXTEND EVENT
  //

  def onRightLift : Unit = 
    jQuery(rexItem).popup("show")

  def onRexPopupVisible : Unit = {

    pane.hotkeysEnabled = false
    jQuery(rexIdInput).focus()

  }

  def onRexPopupHidden : Unit = {

    jQuery(rexIdInput).value("")
    pane.hotkeysEnabled = true
    pane.focus

  }

  def onRexPopupSubmit : Unit = {

    val id = jQuery(rexIdInput).value().asInstanceOf[String]
    jQuery(rexItem).popup("hide")

    run(
      for {
        i <- attempt(activeInstance, "No active instance")
        _ <- i.rightLift(id)
      } yield ()
    )

  }

  //============================================================================================
  // ASSERT RIGHT EVENT
  //

  def onAssertRight : Unit = 
    for { i <- activeInstance } { 
      run(i.assertRightExtension) 
    }

  //============================================================================================
  // UI COMPONENTS
  //

  // Assume Popup Components

  val assumeItem = a(cls := "assume item")("Assume", i(cls := "dropdown icon")).render
  val varIdInput = input(`type` := "text", placeholder := "Identifier").render
  val isLexCheckbox = input(`type` := "checkbox", tabindex := 0, cls := "hidden").render
  val isRexCheckbox = input(`type` := "checkbox", tabindex := 0, cls := "hidden").render
  val frameDisplay = div(cls := "ui basic segment").render

  val assumePopup =
    div(cls := "ui bottom left popup transition hidden")(
      div(cls := "ui basic left aligned segment")(
        form(cls := "ui form")(
          div(cls := "field")(
            label("Assume Variable:"),
            varIdInput
          ),
          div(cls := "field")(
            div(cls := "ui checkbox")(
              isLexCheckbox,
              label("Left Extension")
            )
          ),
          div(cls := "field")(
            div(cls := "ui checkbox")(
              isRexCheckbox,
              label("Right Extension")
            )
          )
        ),
        frameDisplay
      )
    ).render

  // Compose Popup Components

  val composeItem = a(cls := "item")("Compose", i(cls := "dropdown icon")).render
  val compIdInput = input(`type` := "text", placeholder := "Identifier").render

  val composePopup =
    div(cls := "ui bottom left popup transition hidden")(
      div(cls := "ui basic left aligned segment")(
        form(cls := "ui form")(
          div(cls := "field")(
            label("Compose Diagram:"),
            compIdInput
          )
        )
      )
    ).render

  // Compose Popup Components

  val lexItem = a(cls := "item")("Left Extend", i(cls := "dropdown icon")).render
  val lexIdInput = input(`type` := "text", placeholder := "Identifier").render

  val lexPopup =
    div(cls := "ui bottom left popup transition hidden")(
      div(cls := "ui basic left aligned segment")(
        form(cls := "ui form")(
          div(cls := "field")(
            label("Left Extension:"),
            lexIdInput
          )
        )
      )
    ).render

  // Rex Popup Components

  val rexItem = a(cls := "item")("Right Extend", i(cls := "dropdown icon")).render
  val rexIdInput = input(`type` := "text", placeholder := "Identifier").render

  val rexPopup =
    div(cls := "ui bottom left popup transition hidden")(
      div(cls := "ui basic left aligned segment")(
        form(cls := "ui form")(
          div(cls := "field")(
            label("Right Extension:"),
            rexIdInput
          )
        )
      )
    ).render

  // Tabs

  val tabs = div(cls := "ui basic segment", style := "min-height: 310px").render
  val tabLabels = 
    div(cls := "ui center aligned bottom attached segment")(
      a(cls := "ui grey circular label", onclick := { () => newInstance })("+")
    ).render

  // Main UI Element

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
        assumeItem,
        assumePopup,
        composeItem,
        composePopup,
        lexItem,
        lexPopup,
        rexItem,
        rexPopup
      ),
      div(cls := "ui attached segment", style := "overflow: hidden")(
        tabs
      ),
      tabLabels
    ).render

}
