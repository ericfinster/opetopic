/**
  * Prover.scala - Opetopic Theorem Prover
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.tt._
import OpetopicTypeChecker._
import opetopic.js.JQuerySemanticUI._

import syntax.complex._
import syntax.tree._
import syntax.nesting._
import syntax.suite._

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
        runEditorAction(onAssume)
      })

    jQuery("#compose-form").on("submit",
      (e : JQueryEventObject) => {
        e.preventDefault
        runEditorAction(onCompose)
      })

    jQuery("#comp-id-input").on("input", () => {
      val compId = jQuery("#comp-id-input").value().asInstanceOf[String]
      jQuery("#fill-id-input").value(compId ++ "-fill")
      jQuery("#prop-id-input").value(compId ++ "-fill-isLeft")
    })

    contextExtend("X", ECat)
    newEditor

  }

  //============================================================================================
  // EDITOR ROUTINES
  //

  def runEditorAction(act: EditorM[Unit]) : Unit = {

    import scalaz.\/
    import scalaz.\/-
    import scalaz.-\/

    act match {
      case -\/(msg: String) => showErrorMessage(msg)
      case \/-(_) => ()
    }

  }

  var editorCount: Int = 0
  var activeEditor: Option[Editor] = None

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

  def showErrorMessage(str: String) : Unit = {

    val closeIcon = i(cls := "close icon").render

    val msg = div(cls := "ui negative message")(
      closeIcon,
      div(cls := "header")("Error:"),
      p(str)
    ).render

    jQuery(closeIcon).on("click", () => {
      jQuery(msg).transition(lit(
        animation = "fade",
        onComplete = () => {
          jQuery(msg).remove()
        }
      ))
    })

    jQuery("#msg-box").append(msg)

  }

  def showInfoMessage(str: String) : Unit = {

    val closeIcon = i(cls := "close icon").render

    val msg = div(cls := "ui yellow message")(
      closeIcon,
      div(cls := "header")("Info:"),
      p(str)
    ).render

    jQuery(closeIcon).on("click", () => {
      jQuery(msg).transition(lit(
        animation = "fade",
        onComplete = () => {
          jQuery(msg).remove()
        }
      ))
    })

    jQuery("#msg-box").append(msg)

  }

  //============================================================================================
  // CONTEXT MANAGEMENT
  //

  // Map a normal form back to a name and an expression
  val nfMap: HashMap[Expr, (String, Expr)] = HashMap()

  val catExpr: Expr = EVar("X")

  val context: ListBuffer[(String, Expr)] = ListBuffer()
  val environment: ListBuffer[(String, Expr, Expr)] = ListBuffer()

  var gma: List[(Name, TVal)] = Nil
  var rho: Rho = RNil

  def contextExtend[N <: Nat](id: String, ty: Expr) : Unit = {

    // Take care of the semantic part
    val tVal = eval(ty, rho)
    val l = lRho(rho)
    val gen = Nt(Gen(l, "TC#"))

    gma = (id, tVal) :: gma
    rho = UpVar(rho, PVar(id), gen)

    context += ((id, ty))
    nfMap(EVar("TC#" ++ l.toString)) = (id, EVar(id))

    val title = div(cls := "title")(
      i(cls := "dropdown icon"), id ++ " : " ++ PrettyPrinter.prettyPrint(ty)
    ).render

    def mkPasteBtn = 
      button(
        cls := "ui icon button",
        onclick := { () => runEditorAction(onPaste(EVar(id), id)) }
      )(
        i(cls := "paste icon")
      )

    def isPasteable(ty: Expr) : Boolean = 
      ty match {
        case EOb(_) => true
        case ECell(_, _) => true
        case _ => false
      }

    val content = div(cls := "content")(
      if (isPasteable(ty)) mkPasteBtn else p("No information")
    ).render

    jQuery("#context-pane").append(title, content)

  }

  //============================================================================================
  // CELL ASSUMPTIONS
  //

  def onAssume: EditorM[Unit] =
    for {
      editor <- attempt(activeEditor, "No active editor")
      id = jQuery("#assume-id-input").value().asInstanceOf[String]
      _ <- editor.withSelection(new editor.BoxAction[Unit] {

          def objectAction(box: editor.EditorBox[_0]) : EditorM[Unit] = 
            for {
              _ <- forceNone(box.optLabel, "Selected box is already occupied!")
            } yield {

              val ty = EOb(catExpr)
              val mk = ObjectMarker(id, EVar(id))

              contextExtend(id, ty)

              box.optLabel = Some(mk)
              box.panel.refresh
              editor.ce.refreshGallery

            }

          def cellAction[P <: Nat](p: P)(box: editor.EditorBox[S[P]]) : EditorM[Unit] = 
            for {
              _ <- forceNone(box.optLabel, "Selected box is already occupied!")
              frm <- editor.frameComplex(box)
              ty = ECell(catExpr, frm)
              mk = CellMarker(p)(id, EVar(id))
              _ <- runCheck(
                checkT(rho, gma, ty)
              )(
                msg => editorError("Typechecking error: " ++ msg)
              )(
                _ => editorSucceed(())
              )
            } yield {

              contextExtend(id, ty)

              box.optLabel = Some(mk)
              box.panel.refresh
              editor.ce.refreshGallery

            }
        })
    } yield ()

  //============================================================================================
  // COMPOSITION
  //

  def onCompose: EditorM[Unit] =
    for {
      editor <- attempt(activeEditor, "No editor active")
      _ <- editor.withSelection(new editor.BoxAction[Unit] {

        def objectAction(box: editor.EditorBox[_0]) : EditorM[Unit] =
          editorError("Cannot fill an object!")

        def cellAction[P <: Nat](p: P)(fillBox: editor.EditorBox[S[P]]) : EditorM[Unit] =
          for {
            fc <- fromShape(fillBox.faceComplex)
            (fpBoxes, compBox, nchTr) <- (
              fc.tail match {
                case Complex(fpBoxes, Box(compBox, nchTr)) => editorSucceed((fpBoxes, compBox, nchTr))
                case _ => editorError("Malformed composition complex")
              }
            )
            _ <- forceNone(fillBox.optLabel, "Filling box is occupied!")
            _ <- forceNone(compBox.optLabel, "Composite box is occupied!")
            nch <- nchTr.traverse[EditorM, Expr]({
              case nst => 
                for { 
                  b <- attempt(nst.baseValue.optLabel, "Niche is not complete!") 
                } yield b.expr
            })

            // This is pretty damn ugly ...
            fp <- Suite.traverse[EditorM, editor.BNst, editor.ENst, P](fpBoxes)(editor.SuiteExtractExprs)

          } yield {

            val comp = EComp(catExpr, fp, nch)
            val fill = EFill(catExpr, fp, nch)
            val fillLeftExt = EFillIsLeft(catExpr, fp, nch)

            val compId = jQuery("#comp-id-input").value().asInstanceOf[String]
            val fillId = jQuery("#fill-id-input").value().asInstanceOf[String]
            val propId = jQuery("#prop-id-input").value().asInstanceOf[String]

            compBox.optLabel = Some(Marker(p)(compId, comp))
            fillBox.optLabel = Some(Marker(S(p))(fillId, fill))

            compBox.panel.refresh
            fillBox.panel.refresh
            editor.ce.refreshGallery

          }

      })
    } yield ()

  //============================================================================================
  // PASTING
  //

  def onPaste(e: Expr, id: String) : EditorM[Unit] = 
    for {
      editor <- attempt(activeEditor, "No editor active")
      _ <- editor.pasteToCursor(e, eval(catExpr, rho), id)
    } yield ()


}
