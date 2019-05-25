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

  //============================================================================================
  // COLOR CHECKING
  //

  sealed trait ColorMarker
  case class AddrMarker(val fa: FaceAddr) extends ColorMarker
  case class LoopMarker(val src: FaceAddr, val tgt: FaceAddr) extends ColorMarker

  def validColoring[A](c: SComplex[A], cc: SComplex[FaceAddr]): Except[Unit] =
    checkColoring(c, cc.map(AddrMarker(_)))
  
  def checkColoring[A](c: SComplex[A], cc: SComplex[ColorMarker]): Except[Unit] = {

    val coloringDim = c.dim
    val coloredDim = cc.dim

    // println("Coloring dim: " + coloringDim)
    // println("Colored dim: " + coloredDim)

    def ok: Except[Unit] = Xor.Right(())

    cc.head.dotOption match {
      case Some(AddrMarker(headColor)) => {

        if (headColor.codim > 0) {

          println("Recoloring!")

          import scala.collection.mutable.HashMap

          val addrMap : HashMap[FaceAddr, ColorMarker] = HashMap()

          for {
            coloringFace <- attempt(c.addrComplex.face(headColor), "Failed to calculate face in recoloring!")
            _ <- coloringFace.traverseWithAddr((g, l) => {
              if (addrMap.isDefinedAt(g)) {
                addrMap(g) match {
                  case AddrMarker(fa) => { addrMap(g) = LoopMarker(fa, l) ; ok } // Check the order here!
                  case _ => throwError("Duplicate loop marker!")
                }
              } else { addrMap(g) = AddrMarker(l) ; ok }
            })
            recoloring <- cc.traverse({
              case AddrMarker(fa) => attempt(addrMap.get(fa), "Unrecognized face in coloring!")
              case LoopMarker(src, tgt) => {
                (addrMap.get(src), addrMap.get(tgt)) match {
                  case (Some(AddrMarker(sf)), Some(AddrMarker(tf))) => Xor.Right(LoopMarker(sf, tf))
                  case (Some(AddrMarker(sf)), None) => Xor.Right(AddrMarker(sf))
                  case (None, Some(AddrMarker(tf))) => Xor.Right(AddrMarker(tf))
                  case (_, _) => Xor.Left("Error in conflict resolution.")
                }
              }
            })
            // _ = println("Coloring face: " ++ coloringFace.toString)
            // _ = println("Result of recoloring: " ++ recoloring.toString)
            valid <- checkColoring(coloringFace, recoloring)
          } yield valid

        } else {

          if (coloredDim == 0) {

            println("Object case.")

            cc match {
              case ||(SDot(AddrMarker(ThisDim(Nil)))) => ok
              case _ => throwError("Invalid object coloring.")
            }

          } else if (coloringDim == coloredDim) {

            println("Equality case.")

            for {
              frmData <- attempt(cc.cellFrame, "Failed to extract equidimensional frame.")
              (srcs, tgtColor) = frmData
              _ <- if (headColor == ThisDim(Nil)) ok else throwError("Top cell not colored by an identity")
              valid <- tgtColor match {
                case AddrMarker(PrevDim(ThisDim(Nil))) => {

                  // The target is not decorated by a conflict, so check as usual.
                  for {
                    _ <- srcs.traverseWithAddr((srcColor, addr) =>
                      for {
                        _ <- if (srcColor != AddrMarker(PrevDim(ThisDim(Nil)))) ok else throwError("Source was decorated by a codomain")
                        coloredFace <- attempt(cc.face(coloredDim - 1)(SDir(addr) :: Nil), "Failed to get equidimensional colored source face")
                        valid <- checkColoring(c, coloredFace)
                      } yield valid
                    )
                    coloredTgt <- attempt(cc.target, "Failed to get equidimensional colored target")
                    valid <- checkColoring(c, coloredTgt)
                  } yield valid

                }
                case AddrMarker(_) => throwError("Target cell not colored by codomain: " + tgtColor.toString)
                case LoopMarker(src, tgt) => {

                  // What to do here?  Extract the *unique* source and check the source and target when
                  // colored by the source and target addresses we see here.

                  for {
                    _ <- srcs.toList match {
                      case LoopMarker(s, t) :: Nil =>
                        if (s == src && t == tgt) {
                          for {
                            srcFace <- attempt(cc.face(PrevDim(ThisDim(SDir(Nil) :: Nil))), "Failed to calculate source face in loop case")
                            valid <- checkColoring(c, srcFace.withTopValue(AddrMarker(src)))
                          } yield valid
                        } else throwError("Loop marker mismatch!")
                      case _ => throwError("Malformed source in loop case")
                    }
                    tgtFace <- attempt(cc.target, "Failed to calculate target in loop case")
                    valid <- checkColoring(c, tgtFace.withTopValue(AddrMarker(tgt)))
                  } yield valid

                }
              }
            } yield valid

          } else if (coloringDim < coloredDim) {

            println("Degenerate case.")

            // println("Coloring complex: " + c.toString)
            // println("Colored complex: " + cc.toString)

            for {
              _ <- if (headColor != ThisDim(Nil)) throwError("Top cell not colored by an identity") else ok
              frmData <- attempt(cc.cellFrame, "Failed to extract degenerate frame.")
              (srcs, tgtColor) = frmData
              _ <- srcs.traverseWithAddr((srcColor, addr) => {
                for {
                  coloredFace <- attempt(cc.face(coloredDim - 1)(SDir(addr) :: Nil), "Failed to get degenerate colored source face")
                  valid <- checkColoring(c, coloredFace)
                } yield valid
              })
              coloredTgt <- attempt(cc.target, "Failed to get degenerate colored target")
              _ <- if (tgtColor == AddrMarker(ThisDim(Nil))) ok else throwError("Degenerate target is not colored by id")
              valid <- checkColoring(c, coloredTgt)
            } yield valid

          } else throwError("Coloring dimension is too high!")


        }
      }
      case Some(LoopMarker(s, _)) => {

        // When this happens, it means we have encountered a face which is
        // colored by some cell which happens to have a loop in a higher
        // dimension.  But the face of either the source or the target will
        // not see the difference.  Therefore, choosing either one should be
        // correct, and we can just continue as normal in lower dimensions.

        checkColoring(c, cc.withTopValue(AddrMarker(s)))

      }
      case None => throwError("Complex is not a cell!")
    }

  }

}
