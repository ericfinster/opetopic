/**
  * Editor.scala - Playing with a new editor interface
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.editor

import scala.scalajs.js
import scala.scalajs.js.timers._
import org.scalajs.jquery._
import org.scalajs.dom
import scalatags.JsDom.all._
import js.Dynamic.{literal => lit}

import opetopic._
import opetopic.ui._
import opetopic.js._
import opetopic.mtl._
import opetopic.editor.ui._

import JsDomFramework._
import JQuerySemanticUI._

object Editor {

  val editor = new SimpleCardinalEditor[SimpleMarker]()
  val viewer = new SimpleViewer[Option[SimpleMarker]]

  val faceViewer = new SimpleViewer[Option[SimpleMarker]]
  val linkViewer = new SimpleViewer[Option[SimpleMarker]]

  type EditorCell = editor.StableCell
  type ViewerCell = viewer.CellType

  editor.onSelectAsRoot = (c: EditorCell) => { onEditorSelect(c) }
  viewer.onSelectAsRoot = (c: ViewerCell) => { onViewerSelect(c) }

  // UI Elements

  val viewerPane =
    new FixedBottomPane(
      viewer,
      PlainComponent(
        div(cls := "ui inverted menu", style := "margin-top: 0; border-radius: 0;")(
          a(cls := "item", onclick := { () => onPush })(i(cls := "archive icon"), "Push")
        ).render)
    )

  val editorPane =
    new FixedBottomPane(
      editor,
      PlainComponent(
        div(cls := "ui inverted menu", style := "margin-top: 0; border-radius: 0;")(
          a(cls := "item", onclick := { () => onNew })(i(cls := "file outline icon"), "New"),
          a(cls := "item", onclick := { () => editor.editor.extrudeSelection })(i(cls := "square outline icon"), "Extrude"),
          a(cls := "item", onclick := { () => editor.editor.loopAtSelection })(i(cls := "tint icon"), "Drop"),
          a(cls := "item", onclick := { () => editor.editor.sproutAtSelection })(i(cls := "leaf icon"), "Sprout"),
          div(cls := "item")(
            div(cls := "ui input")(input(`type` := "text", id := "label-input", placeholder := "Label ..."))
          ),
          div(cls := "ui dropdown item")(
            div(cls := "text")("Auto Label"),
            i(cls := "dropdown icon"),
            div(cls := "menu")(
              div(cls := "header")("Type"),
              div(cls := "item", onclick := { () => autoLabel(false) })("Numeric"),
              div(cls := "item", onclick := { () => autoLabel(true) })("Alphabetic")
            )
          ),
          div(cls := "right menu")(
            a(cls := "item", onclick := { () => zoomIn })(i(cls := "zoom in icon"), "Zoom In"),
            a(cls := "item", onclick := { () => zoomOut })(i(cls := "zoom out icon"), "Zoom Out")
          )
        ).render)
    )

  val inspectorPane = 
    new FixedBottomPane(
      new VerticalSplitPane(faceViewer, linkViewer),
      PlainComponent(
        div(cls := "ui inverted menu", style := "margin-top: 0; border-radius: 0;")(
          a(cls := "item", onclick := { () => () })(i(cls := "question circle outline icon"), "Something")
        ).render)
    )

  val editorTab = new Tab("editor-tab", editorPane, true)
  val inspectorTab = new Tab("inspector-tab", inspectorPane)
  val colorTab = new Tab("color-tab", PlainComponent(p("This is the coloring tab").render))

  val tabPane = new TabPane(editorTab, inspectorTab, colorTab)

  val vertSplitPane =
    new VerticalSplitPane(viewerPane, tabPane)

  def onEditorSelect(c: EditorCell): Unit =
    for { face <- c.face } {
      viewer.complex = Some(face)
    }

  def onViewerSelect(c: ViewerCell): Unit =
    for { cmplx <- viewer.complex ; face <- c.face } {

      faceViewer.complex = Some(face)

      val linkItr = new FlagIterator[Option[SimpleMarker]](cmplx, Some(c.faceAddress), true, true)

      //  Hacky.  SCardinal needs a method for extracting a face ....
      val s = Suite.fromList(List.fill(face.dim + 2)((None, None))).get
      val card = FlagExtruder.extrudeFrom(linkItr, None)

      val res = card.cardinalComplex.map({
        case Positive() => None
        case Negative() => None
        case Neutral(o) => o
      })

      linkViewer.complex = res.sourceAt(rootCardinalAddr(card.dim).complexAddress)

    }

  def handleResize: Unit = {

    val uiWidth = jQuery("#editor-div").width.toInt
    val uiHeight = jQuery(".content").first.height.toInt

    vertSplitPane.setWidth(uiWidth)
    vertSplitPane.setHeight(uiHeight)

  }

  def updateLabel: Unit =
    for {
      root <- editor.editor.selectionRoot
    } {

      import latex.LatexParser
      import fastparse.all._

      val inputVal = jQuery("#label-input").value().toString

      val labelVal =
        LatexParser.Input.parse(
          inputVal
        ) match {
          case Parsed.Success(value, _) => value
          case Parsed.Failure(_, _, _) => inputVal
        }

      root.label = 
        root.label match {
          case None => Some(SimpleMarker(labelVal)) 
          case Some(SimpleMarker(l, s, td, sd)) =>
            Some(SimpleMarker(labelVal, s, td, sd))
        }

      editor.editor.renderAll
      // onEditorSelect

    }

  def zoomIn: Unit = {
    editor.scale += 0.1
    editor.editor.renderAll
  }

  def zoomOut: Unit = {
    editor.scale -= 0.1
    editor.editor.renderAll
  }

  def onNew: Unit = {
    editor.editor.cardinal = SCardinal[SimpleMarker]()
    editor.editor.renderAll
  }

  // Helper classes for the cell stack

  class CellEntry {

    val previewPane = new SimpleViewer[Option[SimpleMarker]]
    previewPane.scale = 1.0

    val uiElement =
      div(cls := "item")(
        a(cls := "active title")(i(cls := "dropdown icon"), "Untitled"),
        div(cls := "active content")(
          div(cls := "ui grey inverted segment")(previewPane.uiElement)
        )
      ).render

  }

  def onPush: Unit = {

    // So, the point here should be to add the currect face to the
    // face list.  So we need to make a little preview pane for it
    // with some controls for inspecting and so on and push it to
    // the list.

    for {
      face <- viewer.complex
    } {

      val cellEntry = new CellEntry

      jQuery("#editor-left-sidebar").append(cellEntry.uiElement)
      jQuery(cellEntry.previewPane.uiElement).height(75)
      jQuery(cellEntry.previewPane.uiElement).width(200)
      cellEntry.previewPane.complex = Some(face)

    }

  }

  def autoLabel(letters: Boolean): Unit = {
    var lbl: Int = 0
    val card = editor.editor.cardinal
    card.map((n: editor.editor.NeutralCell) => {
      n.label = if (letters) {
        Some(SimpleMarker((lbl + 97).toChar.toString))
      } else {
        Some(SimpleMarker(lbl.toString))
      }
      lbl += 1
    })

    editor.editor.renderAll
    // onEditorSelect
    
  }


  def main: Unit = {

    jQuery("#editor-div").append(vertSplitPane.uiElement)

    jQuery("#label-input").on("input", () => { updateLabel })
    jQuery(".ui.dropdown.item").dropdown(lit(direction = "upward", action = "hide"))
    // kind of a hack to make the tab resize correctly....
    jQuery(".ui.sidebar.menu .item").tab(lit(onVisible = { () => jQuery(dom.window).trigger("resize") }))

    jQuery("#editor-left-sidebar").accordion()

    // Install the resize handler and trigger the event
    // to set defaults ...
    jQuery(dom.window).on("resize", () => { handleResize })
    setTimeout(100){ jQuery(dom.window).trigger("resize") }

    // Render the editor 
    editor.initialize

  }


}
