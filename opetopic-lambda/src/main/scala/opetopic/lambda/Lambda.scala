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

  editor.onSelectAsRoot = (c: EditorCell) => { onEditorSelect(c) }
  
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
          a(cls := "item", onclick := { () => onLift })(i(cls := "external alternate icon"), "Lift"),
          a(cls := "item", onclick := { () => onPaste })(i(cls := "edit outline icon"), "Paste"),
          div(cls := "item")(
            div(cls := "ui action input")(
              input(id := "expr-name-input", `type` := "text", placeholder := "Name ...", onchange := { () => onDefineExpr }),
              button(cls := "ui button")("Define Cell")
            )
          ),
          div(cls := "right menu")(
            div(cls := "item")(
              div(cls := "ui labeled icon button", onclick := { () => onFollowToggle })(i(cls := "close icon", id := "follow-icon"), "Follow Selection")
            ),
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

  var followSelection: Boolean = false

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

  def onEditorSelect(c: EditorCell): Unit =
    for { face <- c.face } {
      if (followSelection) {
        viewer.complex = Some(face)
      }
    }

  def handleResize: Unit = {

    val uiWidth = jQuery("#editor-div").width.toInt
    val uiHeight = jQuery(".content").first.height.toInt

    vertSplitPane.setWidth(uiWidth)
    vertSplitPane.setHeight(uiHeight)

  }

  case class ExprEntry(val ident: String, val expr: Expr) 

  def onNewType: Unit = {

    val typeName = jQuery("#new-type-input").value.asInstanceOf[String]
    logPane.log("Creating a new type called " + typeName)

    val entry = ExprEntry(typeName, Var(||(SBox(Obj, SNode(SDot(Obj), SLeaf))), typeName))
    val uiElement = a(cls := "item",
      onclick := { () => selectExpr(entry) })(typeName).render
    
    jQuery("#editor-left-sidebar").append(uiElement)
    environment += (typeName -> entry)

  }

  def onDefineExpr: Unit = 
    for {
      root <- editor.editor.selectionRoot
      face <- root.face
    } {

      val exprs: List[Option[Expr]] = face.toList

      if (exprs.forall(_.isDefined)) {
        logPane.debug("Ready to save cell")

        val exprName = jQuery("#expr-name-input").value.asInstanceOf[String]
        val expr = face.head.baseValue.get
        logPane.debug("Expression is: " + expr.toString)
        val entry = ExprEntry(exprName, face.head.baseValue.get)
        val uiElement = a(cls := "item",
          onclick := { () => selectExpr(entry) })(exprName).render
    
        jQuery("#editor-left-sidebar").append(uiElement)
        environment += (exprName -> entry)

        logPane.ok("Defined expression " + exprName)

      } else {
        logPane.error("Selected cell is not full.")
      }
    }


  def onLift: Unit = 
    for {
      root <- editor.editor.selectionRoot
      face <- root.face
      bface <- root.boxFace
    } {

      face match {

        //
        // Left Lifting
        //

        case tl >> SBox(None, cn) >> SDot(None) => {

          val m = for {

            web <- attempt(tl.traverseComplex(o => o), "Incomplete base")
            pd <-  attempt(cn.traverse({
              case SDot(o) => o
              case _ => None
            }), "Error extracting pasting diagram.")
            _ <- Expr.validateLeftLift(web, pd)

          } yield (web, pd)

          m match {
            case Xor.Left(msg) => logPane.error(msg)
            case Xor.Right((web, pd)) => {

              logPane.debug("Finished left lift.")

              // The newly created expressions
              val comp = LeftComp(web, pd)
              val fill = LeftFill(web, pd)

              // The destination cells
              val compCell = bface.tail.get.head.baseValue
              val fillCell = bface.head.baseValue

              compCell.label = Some(comp)
              fillCell.label = Some(fill)
              editor.editor.renderAll

            }
          }

        }

        //
        //  Right Lifting
        //

        case tl >> SBox(Some(tgt), cn) >> SDot(None) => {

          val tr = cn.map({
            case SDot(o) => o
            case _ => None
          })

          tr.mapWithAddr({
            case (Some(e), _) => None
            case (_, addr) => Some(addr)
          }).toList.flatten match {
            case addr :: Nil => {

              val m = for {

                // Check that the base is full
                web <- attempt(tl.traverseComplex(o => o), "Incomplete base")
                
                // Generate the appropriate derivative
                z <- attempt(tr.seekTo(addr), "Invalid address")
                noOptZip <- attempt(z.ctxt.close(SLeaf).map(_.get).seekTo(addr), "Internal error")
                noOptShell = z.focus.nodeOption.get._2.map(_.map(_.get))
                deriv = SDeriv(noOptShell, noOptZip.ctxt)

                // Validate the lift
                _ <- Expr.validateRightLift(web, deriv, tgt)

              } yield (web, deriv)

              m match {
                case Xor.Left(msg) => logPane.error(msg)
                case Xor.Right((web, deriv)) => {

                  // The newly created expressions
                  val comp = RightComp(web, deriv, tgt)
                  val fill = RightFill(web, deriv, tgt)

                  // The destination cells
                  val fillCell: EditorCell = bface.head.baseValue
                  val compCell: EditorCell = (for {
                    t <- bface.tail
                    n <- t.head.boxOption
                    m <- n._2.elementAt(addr)
                  } yield m.baseValue).get

                  // Set the new labels and re-render
                  compCell.label = Some(comp)
                  fillCell.label = Some(fill)
                  editor.editor.renderAll
                  
                }
              }
            }
            case _ => logPane.error("Inverse lifting problem does not have a unique empty source.")
          }
        }
        case _ => logPane.error("Malformed lifting condition")
      }

    }

  def onPaste: Unit = 
    for {
      root <- editor.editor.selectionRoot
      face <- root.boxFace
      cell <- viewer.complex
    } {

      face.matchWith(cell) match {
        case None => logPane.error("Incompatible shape.")
        case Some(pc) => {

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
            case Xor.Left(msg) => logPane.error(msg)
            case Xor.Right(cmplx) => {

              // Now traverse the complex and update the labels.
              cmplx.foreach({
                case (c, e) => { c.label = Some(e) }
              })

              // Now re-render ...
              editor.editor.renderAll

            }
          }

        }
      }

    }

  def selectExpr(entry: ExprEntry): Unit = {
    val cmplx: SComplex[Option[Expr]] =
      entry.expr.exprComplex.map(Some(_))
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
