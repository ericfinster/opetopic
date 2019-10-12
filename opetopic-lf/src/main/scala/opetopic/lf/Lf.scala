/**
  * Lf.scala - The opetopic logical framework
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.lf

import scalatags.JsDom.all._
import org.scalajs.jquery._
import org.scalajs.dom
import scala.scalajs.js.timers._

import opetopic._
import opetopic.ui._
import opetopic.js._
import opetopic.js.ui._
import opetopic.mtl._

import JsDomFramework._
import JQuerySemanticUI._

object Lf {

  val logPane = new LogPane()

  var theory: Theory = new OmegaCat(logPane)

  def editor = theory.editor
  def viewer = theory.viewer
    
  val editorPane =
    new FixedBottomPane(
      editor,
      PlainComponent(
        div(cls := "ui inverted menu", style := "margin-top: 0; border-radius: 0;")(
          a(cls := "item", onclick := { () => onNew })(i(cls := "file outline icon"), "New"),
          a(cls := "item", onclick := { () => editor.editor.extrudeSelection })(i(cls := "square outline icon"), "Extrude"),
          a(cls := "item", onclick := { () => editor.editor.loopAtSelection })(i(cls := "tint icon"), "Drop"),
          a(cls := "item", onclick := { () => editor.editor.sproutAtSelection })(i(cls := "leaf icon"), "Sprout"),
          a(cls := "item", onclick := { () => runAction(theory.doLift) })(i(cls := "external alternate icon"), "Lift"),
          a(cls := "item", onclick := { () => runAction(theory.pasteFromViewer) })(i(cls := "edit outline icon"), "Paste"),
          div(cls := "item")(
            div(cls := "ui action input")(
              input(id := "expr-name-input", `type` := "text", placeholder := "Name ...", onchange := { () => theory.onDefineExpr }),
              button(cls := "ui button")("Define Cell")
            )
          ),
          div(cls := "item")(
            div(cls := "ui action input")(
              input(id := "postulate-input", `type` := "text", placeholder := "Id ...", onchange := { () => runAction(theory.doPostulate) }),
              button(cls := "ui button")("Postulate")
            )
          ),
          div(cls := "right menu")(
            a(cls := "item", onclick := { () => zoomIn })(i(cls := "zoom in icon"), "Zoom In"),
            a(cls := "item", onclick := { () => zoomOut })(i(cls := "zoom out icon"), "Zoom Out")
          )
        ).render)
    )

  val viewerPane =
    new FixedBottomPane(
      new HorizontalSplitPane(
        viewer,
        logPane
      ),
      PlainComponent(
        div(cls := "ui inverted menu", style := "margin-top: 0; border-radius: 0;").render
      )
    )

  val vertSplitPane =
    new VerticalSplitPane(editorPane, viewerPane)

  def runAction[A](e: Except[A]): Unit =
    e match {
      case Xor.Left(msg) => logPane.error(msg)
      case Xor.Right(_) => ()
    }

  def onNew: Unit = {
    // editor.editor.cardinal = theory.defaultCardinal
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

  def handleResize: Unit = {

    val uiWidth = jQuery("#editor-div").width.toInt
    val uiHeight = jQuery(".content").first.height.toInt

    vertSplitPane.setWidth(uiWidth)
    vertSplitPane.setHeight(uiHeight)

  }

  def main: Unit = {

    jQuery("#editor-div").append(vertSplitPane.uiElement)

    jQuery(dom.window).on("resize", () => { handleResize })
    setTimeout(100){
      jQuery(dom.window).trigger("resize")
    }

    theory.initialize

  }

}
