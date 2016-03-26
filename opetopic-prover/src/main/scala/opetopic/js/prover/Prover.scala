/**
  * Prover.scala - Opetopic Theorem Prover
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scala.collection.mutable.ListBuffer

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.tt._
import opetopic.js.JQuerySemanticUI._

object Prover extends JSApp {

  def main : Unit = {

    println("Launched Opetopic Prover.")

    jQuery(".ui.accordion").accordion()
    jQuery(".menu .item").tab()
    jQuery("#new-editor").on("click", () => newEditor)

    jQuery("#tab-pane").keypress((e : JQueryEventObject) => {
      e.which match {
        case 101 => for { editor <- activeEditor } { editor.ce.extrudeSelection }
        case 100 => for { editor <- activeEditor } { editor.ce.extrudeDrop }
        case 112 => for { editor <- activeEditor } { editor.ce.sprout }
        case _ => ()
      }
    })

    jQuery(".ui.checkbox").checkbox()

    jQuery("#assume-form").on("submit",
      (e : JQueryEventObject) => {
        e.preventDefault
        runEditorAction(onAssumeSubmit)
      })

    contextExtend("X", ECat)
    newEditor

  }

  var editorCount: Int = 0
  var activeEditor: Option[Editor] = None

  val catExpr: Expr = EVar("X")

  val context: ListBuffer[(String, Expr)] = ListBuffer(("X", ECat))
  val environment: ListBuffer[(String, Expr, Expr)] = ListBuffer()

  def newEditor: Unit = {

    val editor = new Editor
    editorCount += 1

    val cntStr = editorCount.toString
    val tabName = "tab-" ++ cntStr

    val tabItem = a(cls := "item", "data-tab".attr := tabName)(cntStr).render
    val tab = div(cls := "ui tab", "data-tab".attr := tabName)(
      editor.ce.element.uiElement
    ).render

    jQuery("#tab-pager").append(tabItem)
    jQuery("#tab-pane").append(tab)

    jQuery(tabItem).tab(lit(
      onVisible = (s: String) => { activeEditor = Some(editor) }
    ))

    jQuery(tabItem).click()

  }

  def onAssumeSubmit: EditorM[Unit] =
    for {
      editor <- attempt(activeEditor, "No active editor")
      id = jQuery("#assume-id-input").value().asInstanceOf[String]
      _ <- editor.withSelection(new editor.BoxAction[Unit] {

          def objectAction(box: editor.EditorBox[_0]) : EditorM[Unit] = {

            val ty = EOb(catExpr)
            val mk = ObjectMarker(id, EVar(id), ty)

            contextExtend(id, ty)

            box.optLabel = Some(mk)
            box.panel.refresh
            editor.ce.refreshGallery

            editorSucceed(())

          }

          def cellAction[P <: Nat](p: P)(box: editor.EditorBox[S[P]]) : EditorM[Unit] = 
            for {
              frm <- editor.frameComplex(box)
              ty = ECell(catExpr, frm)
              mk = CellMarker(p)(id, EVar(id), ty)
            } yield {

              contextExtend(id, ty)

              box.optLabel = Some(mk)
              box.panel.refresh
              editor.ce.refreshGallery

            }
        })
    } yield ()

  def runEditorAction(act: EditorM[Unit]) : Unit = {

    import scalaz.\/
    import scalaz.\/-
    import scalaz.-\/

    act match {
      case -\/(msg: String) => println("Error: " ++ msg)
      case \/-(_) => ()
    }

  }

  def contextExtend(id: String, ty: Expr) : Unit = {

    val title = div(cls := "title")(i(cls := "dropdown icon"), id).render
    val content = div(cls := "content")(p("Type: " ++ ty.toString)).render

    jQuery("#context-pane").append(title, content)

  }

}
