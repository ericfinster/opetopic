/**
  * JsColorEditor.scala - Base javascript coloreditor implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.mtl._
import opetopic.ui._

import JsDomFramework._
import JQuerySemanticUI._

object FaceAddrRenderable {

  def withComplex[A](cmplx: SComplex[A])(implicit r: Renderable[A]) : Renderable[FaceAddr] =
    new Renderable[FaceAddr] {
      def render(f: UIFramework)(fa: FaceAddr): f.CellRendering = {

        cmplx.elementAt(fa) match {
          case None => f.CellRendering(f.text("*"))
          case Some(a) => r.render(f)(a)
        }

      }
    }

}


class JsColorEditor {

  import FaceAddrRenderable._

  type F = JsDomFramework.type

  var hasColoring: Boolean = false
  var coloringCmplx: SComplex[Int] = ||(SDot(0))

  val coloringEditor = new StableEditor[Int, F](JsDomFramework)(SCardinal(||(SDot(None))))
  val coloringViewer = new SimpleActiveGallery[Int, F](JsDomFramework)(||(SDot(0)))

  var coloredEditor = newColoredEditor

  def newColoredEditor: StableEditor[FaceAddr, F] = 
    new StableEditor[FaceAddr, F](JsDomFramework)(SCardinal(||(SDot(None))))(withComplex(coloringCmplx))

  def initialize: Unit = {

    // Install the key handler
    jQuery(uiElement).keypress((e : JQueryEventObject) => {
      e.which match {
        case 101 => doExtrude
        case 100 => doDrop
        case 115 => doSprout
        case _ => ()
      }
    })

    coloringEditor.renderAll
    jQuery(coloringPane).append(coloringEditor.element.uiElement)

  }

  def doExtrude: Unit = 
    if (hasColoring) coloredEditor.extrudeSelection
    else coloringEditor.extrudeSelection

  def doDrop: Unit =
    if (hasColoring) coloredEditor.loopAtSelection
    else coloringEditor.loopAtSelection

  def doSprout: Unit =
    if (hasColoring) coloredEditor.sproutAtSelection
    else coloringEditor.sproutAtSelection

  def onSelectColoring: Unit = {

    coloringEditor.selectionRoot match {
      case None => ()
      case Some(root) => {
        for {
          face <- root.face
        } {

          var cnt: Int = -1
          val numCmplx : SComplex[Int] =
            face.map(_ => { cnt += 1 ; cnt })

          coloringViewer.setComplex(numCmplx)
          coloringViewer.renderAll

          jQuery(coloringPane).empty().append(coloringViewer.element.uiElement)
          jQuery(coloringMenu).empty().append(newColoringBtn)

          hasColoring = true
          coloringCmplx = numCmplx

          coloredEditor = newColoredEditor
          coloredEditor.renderAll
          jQuery(coloredPane).empty().append(coloredEditor.element.uiElement)

        }
      }
    }
  }

  def onNewColoring: Unit = {
    coloringEditor.cardinal = SCardinal(||(SDot(None)))
    coloringEditor.renderAll
    jQuery(coloringPane).empty().append(coloringEditor.element.uiElement)
    jQuery(coloringMenu).empty().append(selectFaceBtn)
    jQuery(coloredPane).empty().append(noColoringMsg)
    hasColoring = false
    coloringCmplx = ||(SDot(0))
  }

  def onReset: Unit = {
    coloredEditor = newColoredEditor
    coloredEditor.renderAll
    jQuery(coloredPane).empty().append(coloredEditor.element.uiElement)
  }

  def onColorWith: Unit =
    if (!hasColoring) () else {
      (coloredEditor.selectionRoot, coloringViewer.selectionRoot) match {
        case (Some(coloredRoot), Some(coloringRoot)) => {

          val codim = coloringCmplx.dim - coloringRoot.dim
          val color = FaceAddr(codim, coloringRoot.address)

          // println("Go for color checking!")
          // println("Color is: " + color.toString)

          // What's next? We want to build the colored face corresponding
          // to this guy.  So the first step is to extract him.

          for {
            face <- coloredRoot.face
            coloredFace <- face match {
              case ||(SDot(Some(_))) => { println("Face is already colored") ; None }
              case ||(SDot(None)) => Some(||(SDot(color)))
              case tl >> SDot(Some(_)) => { println("Face is already colored") ; None }
              case tl >> SDot(None) => {
                val m : Option[SComplex[FaceAddr]] = for {
                  coloredTail <- ComplexTraverse.traverse[Option, Option[FaceAddr], FaceAddr](tl)(o => o)
                } yield coloredTail >> SDot(color)

                if (m == None) println("Frame is incomplete")

                m
              }
              case _ => { println("Unknown error") ; None }
            }
          } {

            validColoring(coloringCmplx, coloredFace) match {
              case Xor.Right(_) => {
                println("===== Coloring is valid! =====")
                coloredRoot.label = Some(color)
                coloredEditor.renderAll
              }
              case Xor.Left(msg) => println("Invalid coloring: " + msg)
            }

          }

        }
        case _ => println("Select both a colored and coloring face")
      }
    }

  //============================================================================================
  // UI ELEMENTS
  //

  val selectFaceBtn = div(cls := "ui button", onclick := { () => onSelectColoring })("Select Face").render
  val newColoringBtn = div(cls := "ui button", onclick := { () => onNewColoring })("New Coloring").render
  val colorWithBtn = div(cls := "ui button", onclick := { () => onColorWith })("Color With").render
  val resetBtn = div(cls := "ui button", onclick := { () => onReset })("Reset").render

  val coloringMenu = div(cls := "ui segment")(
    selectFaceBtn
  ).render

  val coloringPane = div(cls := "ui center aligned segment").render

  val noColoringMsg = p("No coloring opetope selected").render

  val coloredPane = div(cls := "ui center aligned segment")(
    noColoringMsg
  ).render

  val coloredMenu = div(cls := "ui segment")(colorWithBtn, resetBtn).render

  val uiElement = 
    div(cls := "ui nofocus segments", tabindex := 0)(coloringMenu, coloringPane, coloredPane, coloredMenu).render


}
