/**
  * Lambda.scala - An opetopic implementation of planar lambda calculus
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.lambda

import scala.collection.mutable.Map

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

object Lambda {

  val editor = new SimpleCardinalEditor[Expr]()
  val viewer = new SimpleViewer[Option[Expr]]

  type EditorCell = editor.StableCell
  type ViewerCell = viewer.CellType
  
  val editorPane =
    new FixedBottomPane(
      editor,
      PlainComponent(
        div(cls := "ui inverted menu", style := "margin-top: 0; border-radius: 0;")(
          a(cls := "item", onclick := { () => onNew })(i(cls := "file outline icon"), "New"),
          a(cls := "item", onclick := { () => editor.editor.extrudeSelection })(i(cls := "square outline icon"), "Extrude"),
          a(cls := "item", onclick := { () => editor.editor.loopAtSelection })(i(cls := "tint icon"), "Drop"),
          a(cls := "item", onclick := { () => editor.editor.sproutAtSelection })(i(cls := "leaf icon"), "Sprout"),
          div(cls := "right menu")(
            a(cls := "item", onclick := { () => zoomIn })(i(cls := "zoom in icon"), "Zoom In"),
            a(cls := "item", onclick := { () => zoomOut })(i(cls := "zoom out icon"), "Zoom Out")
          )
        ).render)
    )

  val logPane = new LogPane()

  val viewerPane =
    new FixedBottomPane(
      new HorizontalSplitPane(
        viewer,
        logPane
      ),
      PlainComponent(
        div(cls := "ui inverted menu", style := "margin-top: 0; border-radius: 0;")(
          a(cls := "item", onclick := { () => onPaste })(i(cls := "edit outline icon"), "Paste"),
          div(cls := "right menu")(
            div(cls := "item")(
              div(cls := "ui action input")(
                input(id := "new-type-input", `type` := "text", placeholder := "New Type ...", onchange := { () => onNewType }),
                button(cls := "ui button", onclick := { () => onNewType })("New Type")
              )
            )
          )
        ).render
      )
    )

  val vertSplitPane =
    new VerticalSplitPane(editorPane, viewerPane)

  def onNew: Unit = {
    editor.editor.cardinal = SCardinal[Expr]()
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

  case class ExprEntry(val ident: String, val expr: Expr) {
    def getComplex: SComplex[Expr] =
      Expr.toComplex(expr)
  }

  def onNewType: Unit = {

    val typeName = jQuery("#new-type-input").value.asInstanceOf[String]
    logPane.log("Creating a new type called " + typeName)

    val entry = ExprEntry(typeName, Var(typeName))
    val uiElement = a(cls := "item",
      onclick := { () => selectExpr(entry) })(typeName).render
    
    jQuery("#editor-left-sidebar").append(uiElement)
    environment += (typeName -> entry)

  }

  def onPaste: Unit = 
    for {
      root <- editor.editor.selectionRoot
      face <- root.boxFace
      cell <- viewer.complex
    } {

      face.matchWith(cell) match {
        case None => logPane.logError("Incompatible shape.")
        case Some(pc) => {
          logPane.logDebug("Shapes are compatible.")

          // Verify the equality of all expressions when they
          // are both defined and return the resulting complex
          // of expressions.
          val m: Except[SComplex[(EditorCell, Expr)]] =
            pc.traverseComplex[Except, (EditorCell, Expr)]({
              case (c, exprOpt) => {

                (c.label, exprOpt) match {
                  case (Some(e), Some(f)) => {
                    if (e.equals(f)) {
                      succeed((c, e))
                    } else {
                      throwError("Incompatible expressions.")
                    }
                  }
                  case (None, Some(f)) => succeed((c, f))
                  case (Some(e), None) => succeed((c, e))
                  case (None, None) => throwError("Incomplete expression.")
                }

              }
            })

          m match {
            case Xor.Left(msg) => logPane.logError(msg)
            case Xor.Right(cmplx) => {

              // Now traverse the complex and update the labels.
              cmplx.foreach({
                case (c, e) => { c.label = Some(e) }
              })

              // Now re-render ...
              editor.editor.renderAll

              logPane.logDebug("Paste successful.")

            }
          }

        }
      }

    }

  def selectExpr(entry: ExprEntry): Unit = {
    val cmplx: SComplex[Option[Expr]] =
      entry.getComplex.map(Some(_))
    viewer.complex = Some(cmplx)
  }

  val environment: Map[String, ExprEntry] = Map()

  def main: Unit = {

    jQuery("#editor-div").append(vertSplitPane.uiElement)

    jQuery(dom.window).on("resize", () => { handleResize })
    setTimeout(100){ jQuery(dom.window).trigger("resize") }

    // Render the editor 
    editor.initialize

  }

}
