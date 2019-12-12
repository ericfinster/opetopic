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

  var theory: Theory = new CompUniqueGroupoid(logPane)

  def editor = theory.editor
  def viewer = theory.viewer
    
  val editorPane =
    new FixedBottomPane(
      editor,
      PlainComponent(
        div(cls := "ui inverted menu", style := "margin-top: 0; border-radius: 0;")(
          div(cls := "item")(
            div(cls := "ui action input")(
              input(id := "expr-name-input", `type` := "text", placeholder := "Name", onchange := { () => theory.onDefineExpr }),
              button(cls := "ui button")("Define Cell")
            )
          ),
          div(cls := "item")(
            div(cls := "ui action input")(
              input(id := "postulate-input", `type` := "text", placeholder := "Name", onchange := { () => runAction(theory.doPostulate) }),
              button(cls := "ui button")("Postulate Cell")
            )
          ),
          a(cls := "item", onclick := { () => runAction(theory.doLift) })(i(cls := "external alternate icon"), "Lift"),
          a(cls := "item", onclick := { () => runAction(theory.pasteFromViewer) })(i(cls := "edit outline icon"), "Paste")
        ).render)
    )

  val hPane = new HorizontalSplitPane(viewer, logPane)

  val viewerPane =
    new FixedBottomPane(
      hPane,
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
      hPane.initialize
      vertSplitPane.initialize
    }

    theory.initialize
  }

}
