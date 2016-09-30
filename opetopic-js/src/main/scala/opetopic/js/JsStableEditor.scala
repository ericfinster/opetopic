/**
  * JsStableEditor.scala - Javascript Stable Editor Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.collection.mutable.Buffer

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.ui._
import opetopic.mtl._
import JsDomFramework._
import JQuerySemanticUI._

class JsStableEditor[A: Renderable] {

  type StableCell = StableEditor[A, JsDomFramework.type]#EditorCell

  var onSelectAsRoot: StableCell => Unit = { _ => () }

  //============================================================================================
  // TAB CLASS
  //

  class EditorTab(c: SCardinal[Option[A]]) {

    val editor = new StableEditor[A, JsDomFramework.type](JsDomFramework)(c)

    editor.onCellClick = 
      (c: editor.EditorCell) => { }

    editor.onSelectAsRoot = 
      (c: StableCell) => onSelectAsRoot(c)

    def refreshDimensions: Unit = {
      editor.galleryViewport.width = tabWidth
      editor.galleryViewport.height = tabHeight
    }

    editor.renderAll
    refreshDimensions

  }

  //============================================================================================
  // SHAPE ACTIONS
  //

  def doExtrude: Unit = 
    for { tab <- activeTab } {
      tab.editor.extrudeSelection
    }

  def doDrop: Unit =
    for { tab <- activeTab } {
      tab.editor.loopAtSelection
    }

  def doSprout: Unit =
    for { tab <- activeTab } {
      tab.editor.sproutAtSelection
    }

  def doExtract: Unit =
    for {
      tab <- activeTab
      ed = tab.editor
      pd <- ed.extractSelection
      // _ = println("Extracted selection: " + pd.map(_.label).toString)
      ccmplx = ed.complex
      gpd <- pd.traverse((n: ed.NeutralCell) => {

        val addr = n.cardinalAddress.complexAddress
        val codim = ccmplx.dim - n.dim
        val fa = FaceAddr(codim, addr)

        for {
          face <- ccmplx.face(fa)
        } yield (face : SComplex[ed.EditorCell]).map(_.label)

      })
      // _ = println("About to graft")
      pr <- graft(gpd)({ case (fst, _) => Some(fst) })
      (web, srcs) = pr
    } {

      // println("Finished graft.")

      // val newComplex : SComplex[Option[A]] =
      //   web >> SBox(None, srcs) >> SDot(None)

      newEditor(SCardinal(web, srcs))

    }

  //============================================================================================
  // ACTIONS
  //

  def updateLabel(f: Option[A] => Option[A]): Unit = 
    for {
      tab <- activeTab
      root <- tab.editor.selectionRoot
    } {
      root.label = f(root.label)
      refreshEditor
      // root.selectAsRoot
    }

  def rootAction(f: StableCell => Unit): Unit = 
    for {
      tab <- activeTab
      root <- tab.editor.selectionRoot
    } { f(root) }

  def withRoot[B](f: StableCell => Option[B]): Option[B] = 
    for {
      tab <- activeTab
      root <- tab.editor.selectionRoot
      res <- f(root)
    } yield res

  def rootFace: Option[SComplex[Option[A]]] =
    withRoot(_.face)

  //============================================================================================
  // EDITOR MANAGEMENT
  //

  val tabs: Buffer[EditorTab] = Buffer()

  var tabCount: Int = 0
  var activeTab: Option[EditorTab] = None

  def newEditor: Unit = newEditor(SCardinal[A]())
  def newEditor(c: SCardinal[Option[A]]) : Unit = {

    val editorTab = new EditorTab(c)
    tabs += editorTab
    tabCount += 1

    val cntStr = tabCount.toString
    val tabName = "tab-" ++ cntStr

    val tabItem = a(cls := "item", attr("data-tab") := tabName)(cntStr).render
    val tab = div(cls := "ui tab", attr("data-tab") := tabName)(
      editorTab.editor.element.uiElement
    ).render

    jQuery(paginationMenu).append(tabItem)
    jQuery(tabPane).append(tab)

    jQuery(tabItem).tab(lit(
      onVisible = (s: String) => { activeTab = Some(editorTab) }
    ))

    jQuery(tabItem).click()

  }

  def refreshEditor: Unit = 
    for {
      tab <- activeTab
    } { tab.editor.renderAll }

  //============================================================================================
  // UI ELEMENTS
  //

  val tabPane = div(cls := "ui middle attached nofocus segment", tabindex := 0, style := "min-height: 300px").render
  val paginationMenu = div(cls := "ui pagination menu").render
  
  val topMenu =
    div(cls := "ui top attached menu")(
      div(cls := "ui dropdown item")(
        "Shape", i(cls := "dropdown icon"),
        div(cls := "vertical fluid menu")(
          div(cls := "item", style := "min-width: 150px", onclick := { () => doExtrude })(span(cls := "description")("e"), "Extrude"),
          div(cls := "item", onclick := { () => doDrop })(span(cls := "description")("d"), "Drop"),
          div(cls := "item", onclick := { () => doSprout })(span(cls := "description")("s"), "Sprout")
        )
      )
    ).render

  val bottomMenu =
    div(cls := "ui bottom attached segment")(
      div(cls := "ui grid")(
        div(cls := "four column row")(
          div(cls := "left floated column")(
            paginationMenu
          ),
          div(cls := "right floated right aligned column")(
            button(cls := "ui icon button", onclick := { () => newEditor })(i(cls := "add icon"))
          )
        )
      )
    ).render

  val uiElement =
    div(topMenu, tabPane, bottomMenu).render


  //============================================================================================
  // UI STATE
  //

  var tabWidth : Int = 0
  var tabHeight : Int = 0

  //============================================================================================
  // INITIALIZATION
  //

  def initialize: Unit = {

    // The idea is that we're going to hook the resize event
    // on the parent element and manage the viewport ourselves..

    tabWidth = jQuery(tabPane).width.toInt
    tabHeight = jQuery(tabPane).height.toInt

    // Install the key handler
    jQuery(uiElement).keypress(handleKeyEvent(_))

    jQuery(topMenu).
      find(".dropdown.item").
      dropdown(lit(action = "hide"))

    jQuery(dom.window).on("resize", () => { resizeInstances })

    newEditor

  }

  def handleKeyEvent(ev: JQueryEventObject): Unit = {
    ev.which match {
      case 101 => doExtrude
      case 100 => doDrop
      case 115 => doSprout
      case 120 => doExtract
      case _ => ()
    }
  }

  def resizeInstances: Unit = {

    tabWidth = jQuery(tabPane).width.toInt

    for {
      tab <- tabs
    } { tab.refreshDimensions }

  }

}
