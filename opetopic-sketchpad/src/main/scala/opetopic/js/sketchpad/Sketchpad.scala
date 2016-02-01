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
    addEditorTab

  }

  var tabCount: Int = 0

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

  val propertyGalleryConfig: GalleryConfig =
    GalleryConfig(
      panelConfig = defaultPanelConfig,
      width = 1000,
      height = 85,
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

      val gallery = ActiveGallery(propertyGalleryConfig, lblCmplx)(
        new AffixableFamily[tab.editor.OptA] {
          def apply[N <: Nat](n: N) : Affixable[tab.editor.OptA[N]] =
            Affixable.optionAffixable(propertyGalleryConfig.spacerBounds, tab.editor.r(n))
        }
      )

      jQuery("#face-pane").empty().append(gallery.element.uiElement)

    }

}
