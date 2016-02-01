/**
  * Sketchpad.scala - Opetopic Sketchpad Application
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import scala.scalajs.{js => sjs}
import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic._
import opetopic.js._
import syntax.complex._
import JsDomFramework._

// import org.denigma.codemirror.extensions.EditorConfig
// import org.denigma.codemirror.{CodeMirror, EditorConfiguration}
// import org.scalajs.dom.raw.HTMLTextAreaElement

import opetopic.js.JQuerySemanticUI._

object Sketchpad extends JSApp {

  // Setup CodeMirror

  // val params : EditorConfiguration = 
  //   EditorConfig.lineNumbers(true) 

  // val codeArea = 
  //   document.getElementById("code-area").
  //     asInstanceOf[HTMLTextAreaElement]

  // val editor = CodeMirror.fromTextArea(codeArea, params)

  def main : Unit = {

    println("Launched Opetopic Sketchpad.")

    jQuery("#new-tab").click((e : JQueryEventObject) => { addEditorTab })
    jQuery("#sketch-prop-tab-menu .item").tab()

    jQuery("#fill-color-btn").popup(sjs.Dynamic.literal(
      popup = jQuery(".color-select.popup"),
      movePopup = false,
      on = "click",
      onShow = () => { isFill = true }
    ))

    jQuery("#stroke-color-btn").popup(sjs.Dynamic.literal(
      popup = jQuery(".color-select.popup"),
      movePopup = false,
      on = "click",
      onShow = () => { isFill = false }
    ))

    jQuery("#label-input").on("input", () => {
      updateLabel
    })

    jQuery(".color-select.popup button").on("click", (e: JQueryEventObject) => {
      val colorStr = jQuery(e.target).attr("data-color").toString

      if (isFill) {
        jQuery("#fill-color-btn").removeClass(fillColor).addClass(colorStr).popup("hide")
        fillColor = colorStr
      } else {
        jQuery("#stroke-color-btn").removeClass(strokeColor).addClass(colorStr).popup("hide")
        strokeColor = colorStr
      }
    })

    addEditorTab

  }

  var tabCount: Int = 0

  var isFill: Boolean = true
  var fillColor: String = "white"
  var strokeColor: String = "black"

  def addEditorTab: Unit = {

    val editorTab = new EditorTab

    tabCount += 1
    val tc = tabCount.toString
    val tabName = "tab-" ++ tc

    val tabItem = a(cls := "item", "data-tab".attr := tabName)(tc).render
    val tab = div(cls := "ui tab", "data-tab".attr := tabName)(
      editorTab.uiElement
    ).render

    jQuery(".right.menu").before(tabItem)
    jQuery("#sketch-tabs").append(tab)

    jQuery(tabItem).tab(sjs.Dynamic.literal(
      onVisible = (s: String) => { activeTab = Some(editorTab) }
    ))

    jQuery(tabItem).click()

  }

  def unescapeUnicode(str: String): String =
    """\\u([0-9a-fA-F]{4})""".r.replaceAllIn(str,
      m => Integer.parseInt(m.group(1), 16).toChar.toString)

  def updateLabel: Unit = 
    for {
      tab <- activeTab
      bs <- tab.activeBox
    } {

      val box = bs.value

      val labelVal = jQuery("#label-input").value().toString

      if (labelVal == "") {
        box.optLabel = None
      } else {

        val mk = CellMarker[bs.N](unescapeUnicode(labelVal), "white", "black")
        box.optLabel = Some(mk)
      }

      box.panel.refresh
      tab.editor.refreshGallery

    }

  val propertyGalleryConfig: GalleryConfig =
    GalleryConfig(
      panelConfig = defaultPanelConfig,
      width = 950,
      height = 150,
      spacing = 1500,
      minViewX = Some(60000),
      minViewY = Some(6000),
      spacerBounds = Bounds(0, 0, 600, 600)
    )

  var activeTab: Option[EditorTab] = None

  def refreshFacePreview: Unit = 
    for {
      tab <- activeTab
      bs <- tab.activeBox
      lblCmplx <- bs.value.labelComplex
    } {

      bs.value.optLabel match {
        case None => jQuery("#label-input").value("")
        case Some(mk) => jQuery("#label-input").value(mk.label)
      }

      // This is super ugly ...
      val gallery = ActiveGallery(propertyGalleryConfig, lblCmplx)(
        new AffixableFamily[tab.editor.OptA] {
          def apply[N <: Nat](n: N) : Affixable[tab.editor.OptA[N]] =
            Affixable.optionAffixable(propertyGalleryConfig.spacerBounds, tab.editor.r(n))
        }
      )

      jQuery("#face-pane").empty().append(gallery.element.uiElement)

    }

}
