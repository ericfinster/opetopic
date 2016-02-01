/**
  * SketchPane.scala - An Editor Pane component
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic._
import opetopic.ui._
import opetopic.js._
import opetopic.pprint._
import syntax.complex._
import syntax.cardinal._
import JsDomFramework._
import JQuerySemanticUI._
import OpetopicTTPrinter._

class SketchPane {

  sealed trait PaneMode
  case object LabelMode extends PaneMode
  case object DeformMode extends PaneMode

  private var mode : PaneMode = DeformMode

  val baseConfig: GalleryConfig =
    GalleryConfig(
      panelConfig = defaultPanelConfig,
      width = 1000,
      height = 85,
      spacing = 1500,
      minViewX = Some(60000),
      minViewY = Some(6000),
      spacerBounds = Bounds(0, 0, 600, 600)
    )

  val editor = CardinalEditor[ConstString]
  editor.onSelectAsRoot = showBoxProperties

  val topMenuElement = 
    div(cls := "ui top attached menu raised")(
      div(cls := "ui dropdown simple item")(
        "Shape",
        i(cls := "dropdown icon"),
        div(cls := "menu")(
          a(cls := "item")("Extrude"),
          a(cls := "item")("Drop"),
          a(cls := "item")("Precomp")
        )
      ),
      div(cls := "ui dropdown simple item")(
        "Export",
        i(cls := "dropdown icon"),
        div(cls := "menu")(
          a(cls := "item")("To Scala"),
          a(cls := "item", onclick := { () => () /* showOpetopicCode */ })("To OpetopicTT"),
          div(cls := "divider"),
          a(cls := "item")("As Png"),
          a(cls := "item")("As Svg")
        )
      ),
      div(cls := "ui item")(
        div(cls := "ui buttons")(
          button(id := "deform-mode-btn", cls := "ui labeled active icon button", onclick := { () => setDeformMode })(
            i(cls := "cubes icon"), "Deform"
          ),
          button(id := "label-mode-btn", cls := "ui labeled icon button", onclick := { () => setLabelMode })(
            i(cls := "tag icon"), "Label"
          )
        )
      )
    ).render

  val paneElement = 
    div(cls := "ui attached center aligned segment")(editor.element.uiElement).render

  val facePaneElement = 
    div(cls := "ui bottom attached raised center aligned segment").render

  val uiElement = div(tabindex := 0)(
    topMenuElement,
    paneElement,
    facePaneElement,
    div()
  ).render

  jQuery(uiElement).keydown((e : JQueryEventObject) => {
    if (e.which == 8) {
      e.preventDefault
      if (mode == LabelMode) deleteFromLabel
    }
  }).keypress((e : JQueryEventObject) => {
    mode match {
      case DeformMode => 
        e.which match {
          case 101 => editor.extrudeSelection
          case 100 => editor.extrudeDrop
          case 112 => editor.sprout
          case _ => ()
        }
      case LabelMode => {
        val c = e.which.toChar
        if (c.isLetterOrDigit) appendToLabel(c)
      }
    }
  })

  def setLabelMode: Unit = {
    mode = LabelMode
    jQuery(uiElement).find("#deform-mode-btn").removeClass("active")
    jQuery(uiElement).find("#label-mode-btn").addClass("active")
  }

  def setDeformMode: Unit = {
    mode = DeformMode
    jQuery(uiElement).find("#label-mode-btn").removeClass("active")
    jQuery(uiElement).find("#deform-mode-btn").addClass("active")
  }

  // def showOpetopicCode: Unit = 
  //   for {
  //     boxsig <- currentBox
  //     lblCmplx <- boxsig.value.labelComplex
  //   } {
  //     Sketchpad.editor.getDoc().setValue(pprintComplex(lblCmplx))
  //     jQuery(".ui.modal").modal("show")
  //     Sketchpad.editor.refresh()
  //   }

  var currentBox: Option[Sigma[editor.CardinalCellBox]] = None

  def deleteFromLabel: Unit = 
    for {
      boxsig <- currentBox
      box = boxsig.value
    } {

      val curLabel: String = 
        box.optLabel.getOrElse("")

      val trimmedLabel: String = 
        if (curLabel.length > 0)
          curLabel.init
        else
          curLabel

      box.optLabel = if (trimmedLabel == "") None else Some(trimmedLabel)

      box.panel.refresh
      editor.refreshGallery

    }

  def appendToLabel(c: Char) : Unit =
    for {
      boxsig <- currentBox
      box = boxsig.value
    } {

      val curLabel: String = 
        box.optLabel.getOrElse("")

      box.optLabel = Some(curLabel + c)
      box.panel.refresh
      editor.refreshGallery

    }

  def showBoxProperties(boxsig: Sigma[editor.CardinalCellBox]) : Unit = {

    currentBox = Some(boxsig)
    val box = boxsig.value

    for {
      lblCmplx <- box.labelComplex
    } {

      val gallery = ActiveGallery(baseConfig, lblCmplx)(
        new AffixableFamily[editor.OptA] {
          def apply[N <: Nat](n: N) : Affixable[editor.OptA[N]] =
            Affixable.optionAffixable(baseConfig.spacerBounds, editor.r(n))
        }
      )

      val div = facePaneElement

      val lc = div.lastChild
      if (lc != null) div.removeChild(lc)

      div.appendChild(gallery.element.uiElement)

    }

  }


}
