/**
  * Editor.scala - Playing with a new editor interface
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.editor

import scala.scalajs.js
import org.scalajs.jquery._
import org.scalajs.dom
import scalatags.JsDom.all._
import js.Dynamic.{literal => lit}

import opetopic._
import opetopic.ui._
import opetopic.js._

import JsDomFramework._
import JQuerySemanticUI._
import SplitPane._

object Editor {

  val editor = new SimpleCardinalEditor[SimpleMarker]()
  val viewer = new SimpleViewer[Option[SimpleMarker]]

  type EditorCell = editor.StableCell

  editor.onSelectAsRoot = (c: EditorCell) => {
    showRootFace
  }

  // UI Elements

  val topPane =
    div(id := "top-pane")(
      editor.uiElement,
      div(cls := "ui inverted menu", style := "margin-top: 0; border-radius: 0;")(
        a(cls := "item")(i(cls := "square outline icon"), "Extrude"),
        a(cls := "item")(i(cls := "tint icon"), "Drop"),
        a(cls := "item")(i(cls := "leaf icon"), "Sprout"),
        a(cls := "item")(i(cls := "cut icon"), "Extract"),
        div(cls := "right menu")(
          a(cls := "item", onclick := { () => zoomIn })(i(cls := "zoom in icon"), "Zoom In"),
          a(cls := "item", onclick := { () => zoomOut })(i(cls := "zoom out icon"), "Zoom Out")
        )
      )
    ).render

  val divider = div(id := "divider",
    style := "min-height: 5px; background: #36383a; cursor: ns-resize;"
  ).render

  val bottomPane = 
    div(id := "bottom-pane")(
      viewer.uiElement
    ).render

  val uiElement =
    div(cls := "split-pane", style := "height: 100%;")(
      topPane,
      divider,
      bottomPane
    ).render

  def showRootFace: Unit =
    for {
      root <- editor.editor.selectionRoot
      face <- root.face
    } {
      viewer.complex = Some(face)
      // Have to resize the viewer here, since it will be overwritten ...
    }

  def handleResize: Unit = {

    val uiWidth = jQuery(uiElement).width.toInt
    val uiHeight = jQuery(".content").first.height.toInt

    val topInnerHeight = .5 * uiHeight

    // println("Resizing with uiHeight: " + uiHeight.toString)
    // println("and uiWidth: " + uiWidth.toString)

    jQuery(editor.uiElement).width(uiWidth)
    jQuery(editor.uiElement).height(.5 * uiHeight)
    editor.resizeViewport

    val usedHeight = jQuery(topPane).height() + jQuery(divider).height()

    jQuery(viewer.uiElement).width(uiWidth)
    jQuery(viewer.uiElement).height(uiHeight - usedHeight)
    viewer.resizeViewport

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
      // showRootFace

    }

  def zoomIn: Unit = {
    editor.scale += 0.1
    editor.editor.renderAll
  }

  def zoomOut: Unit = {
    editor.scale -= 0.1
    editor.editor.renderAll
  }

  def main: Unit = {

    jQuery("#editor-div").append(uiElement)
    jQuery("#label-input").on("input", () => { updateLabel })

    // Render the editor 
    editor.initialize

    // Install the resize handler and trigger the event
    // to set defaults ...
    jQuery(dom.window).on("resize", () => { handleResize })
    jQuery(dom.window).trigger("resize")


  }


}
