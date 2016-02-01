/**
  * Sketchpad.scala - Opetopic Sketchpad Application
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.jquery._
import scalatags.JsDom.all._

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

    // addEditorTab
    jQuery("#new-tab").click((e : JQueryEventObject) => { addEditorTab })

  }

  var tabCount: Int = 0

  def addEditorTab: Unit = {

    val editorTab = new EditorTab

    val tabName = "tab-" ++ tabCount.toString
    tabCount += 1

    val tabItem = div(cls := "item", "data-tab".attr := tabName)("Untitled").render
    val tab = div(cls := "ui tab", "data-tab".attr := tabName)(
      editorTab.uiElement
    ).render

    jQuery(".right.menu").before(tabItem)
    jQuery("#sketch-tabs").append(tab)

    jQuery(tabItem).tab()
    jQuery(tabItem).click()

  }


  // def newInstance: Unit = {

  //   val instance = new EditorInstance(pane, env)
  //   instanceCount += 1

  //   val icStr = instanceCount.toString
  //   val tabName = "tab-" ++ icStr

  //   val tab = div(cls := "ui tab", "data-tab".attr := tabName)(
  //     instance.editor.element.uiElement
  //   ).render

  //   val label = 
  //     a(cls := "ui grey circular label", 
  //       onclick := { () => { jQuery(tab).tab("change tab", tabName) ; activeInstance = Some(instance) } }
  //     )(icStr).render

  //   jQuery(tabs).append(tab)
  //   jQuery(tabLabels).append(label)
  //   jQuery(tab).tab("change tab", tabName)

  //   activeInstance = Some(instance)

  // }



}
