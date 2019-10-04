/**
  * Lambda.scala - An opetopic implementation of planar lambda calculus
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.lambda

import scalatags.JsDom.all._
import org.scalajs.jquery._
import org.scalajs.dom
import scala.scalajs.js.timers._

import opetopic._
import opetopic.ui._
import opetopic.js._
import opetopic.js.ui._

import JsDomFramework._
import JQuerySemanticUI._

object Lambda {

  val editor = new SimpleCardinalEditor[SimpleMarker]()
  val viewer = new SimpleViewer[Option[SimpleMarker]]

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
          div(cls := "right menu")(
            div(cls := "item")(
              div(cls := "ui labeled icon button", onclick := { () => onFollowToggle })(i(cls := "check icon", id := "follow-icon"), "Follow Selection")
            ),
            a(cls := "item", onclick := { () => zoomIn })(i(cls := "zoom in icon"), "Zoom In"),
            a(cls := "item", onclick := { () => zoomOut })(i(cls := "zoom out icon"), "Zoom Out")
          )
        ).render)
    )

  val viewerPane =
    new FixedBottomPane(
      viewer,
      PlainComponent(
        div(cls := "ui inverted menu", style := "margin-top: 0; border-radius: 0;").render
      )
    )

  val vertSplitPane =
    new VerticalSplitPane(editorPane, viewerPane)

  def onNew: Unit = {
    editor.editor.cardinal = SCardinal[SimpleMarker]()
    editor.editor.renderAll
  }

  def zoomIn: Unit = {
    editor.scale += 0.1
    editor.editor.renderAll
  }

  def zoomOut: Unit = {
    editor.scale -= 0.1
    editor.editor.renderAll
  }

  var followSelection: Boolean = true

  def onFollowToggle: Unit = {
    if (followSelection) {
      followSelection = false
      jQuery("#follow-icon").removeClass("check")
      jQuery("#follow-icon").addClass("close")
    } else {
      followSelection = true
      jQuery("#follow-icon").removeClass("close")
      jQuery("#follow-icon").addClass("check")
    }
  }

  def handleResize: Unit = {

    val uiWidth = jQuery("#editor-div").width.toInt
    val uiHeight = jQuery(".content").first.height.toInt

    vertSplitPane.setWidth(uiWidth)
    vertSplitPane.setHeight(uiHeight)

  }

  def main: Unit = {

    jQuery("#editor-div").append(vertSplitPane.uiElement)

    jQuery(dom.window).on("resize", () => { handleResize })
    setTimeout(100){ jQuery(dom.window).trigger("resize") }

    // Render the editor 
    editor.initialize

  }

}
