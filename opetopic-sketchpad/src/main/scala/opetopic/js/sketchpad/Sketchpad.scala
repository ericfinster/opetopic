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
import opetopic.ui._
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

      val color= jQuery(e.target).attr("data-color").toString

      if (isFill) {

        jQuery("#fill-color-btn").removeClass(fillColor).addClass(color).popup("hide")
        fillColor = color
        updateFillColor

      } else {
        jQuery("#stroke-color-btn").removeClass(strokeColor).addClass(color).popup("hide")
        strokeColor = color
      }

    })

    jQuery("#snapshot-btn").on("click", () => { takeSnapshot })

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

      box.optLabel match {
        case None => box.optLabel = Some(CellMarker(unescapeUnicode(labelVal)))
        case Some(mk) => box.optLabel = Some(mk.copy(label = unescapeUnicode(labelVal)))
      }

      box.panel.refresh
      tab.editor.refreshGallery

    }

  def updateFillColor: Unit = 
    for {
      tab <- activeTab
      bs <- tab.activeBox
    } {

      val (f, fh, fs) = CellMarker.colorTripleGen(fillColor)

      bs.value.optLabel match {
        case None => bs.value.optLabel = Some(CellMarker("", 
          DefaultColorSpec.copy(fill = f, fillHovered = fh, fillSelected = fs)
        ))
        case Some(mk) => bs.value.optLabel = Some(mk.copy(
          colorSpec = mk.colorSpec.copy(fill = f, fillHovered = fh, fillSelected = fs)
        ))
      }

      bs.value.panel.refresh
      tab.editor.refreshGallery

    }

  val propertyGalleryConfig: GalleryConfig =
    GalleryConfig(
      panelConfig = defaultPanelConfig,
      width = 900,
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
      implicit val bnds = propertyGalleryConfig.spacerBounds
      val gallery = ActiveGallery(propertyGalleryConfig, lblCmplx)(
        VisualizableFamily.optionVisualizableFamily(bnds, tab.editor.v)
      )

      jQuery("#face-pane").empty().append(gallery.element.uiElement)

    }

  def takeSnapshot: Unit = 
    for {
      tab <- activeTab
      bs <- tab.activeBox
      lblCmplx <- bs.value.labelComplex
    } {

      val exporter = new SvgExporter(lblCmplx)

      jQuery(".ui.modal.svgexport").find("#exportlink").
        attr(sjs.Dynamic.literal(href = "data:text/plain;charset=utf-8," ++
          sjs.URIUtils.encodeURIComponent(exporter.svgString)))

      jQuery(".ui.modal.svgexport").modal("show")

    }
}
