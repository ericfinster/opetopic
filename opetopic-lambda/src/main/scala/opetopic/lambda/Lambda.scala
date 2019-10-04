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
          div(cls := "item")(
            div(cls := "ui action input")(
              input(id := "expr-name-input", `type` := "text", placeholder := "Name ...", onchange := { () => onDefineExpr }),
              button(cls := "ui button")("Save Cell")
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

  def onDefineExpr: Unit = 
    for {
      root <- editor.editor.selectionRoot
      face <- root.face
    } {

      val exprs: List[Option[Expr]] = face.toList

      if (exprs.forall(_.isDefined)) {
        logPane.debug("Ready to save cell")

        val exprName = jQuery("#expr-name-input").value.asInstanceOf[String]
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

      // Extract a pasting diagram by requiring all expressions
      // to be present and external
      def toPd(cn: STree[SNesting[Option[Expr]]]): Option[STree[Expr]] =
        cn.traverse({
          case SDot(o) => o
          case _ => None
        })

      // Return a list of addresses which are non defined.
      def emptyAddresses[A](tr: STree[Option[A]]): List[SAddr] = {
        tr.mapWithAddr({
          case (Some(a), _) => None
          case (None, addr) => Some(addr)
        }).toList.flatten 
      }

      def extractDerivative(tr: STree[Option[Expr]]): Except[(SDeriv[Expr], SAddr)] =
        emptyAddresses(tr) match {
          case addr :: Nil => {
            for {
              z <- attempt(tr.seekTo(addr), "Invalid address")
              // Remove options in the rest of the tree
              noOptZip <- attempt(z.ctxt.close(SLeaf).map(_.get).seekTo(addr), "Internal error")
              noOptShell = z.focus.nodeOption.get._2.map(_.map(_.get))
              deriv = SDeriv(noOptShell, noOptZip.ctxt)
            } yield (deriv, addr)
          }
          case _ => throwError("Inverse lift does not have unique empty source.")
        }

      face match {
        case tl >> SBox(None, cn) >> SDot(None) => {

          // BUG: this will fail in the case of the identity
          // on an object.  We need to make sure objects are
          // always there somehow ....
          val exprTail = tl.traverseComplex(opt => opt)
          
          (toPd(cn), exprTail) match {
            case (None, _) => logPane.error("Incomplete lifting problem.")
            case (Some(pd), Some(exprs)) => {

              // The destination cells
              val tgtCell: EditorCell = bface.tail.get.head.baseValue
              val fillCell: EditorCell = bface.head.baseValue

              if (tl.dim == 0) {

                logPane.debug("Product construction.")

                val prod = Prod(exprs, pd)
                val pair = Pair(exprs, pd)

                tgtCell.label = Some(prod)
                fillCell.label = Some(pair)

              } else if (tl.dim == 1) {

                logPane.debug("Composite construction.")

                // Okay, should be ready for this now....
                val comp = Comp(exprs, pd)
                val fill = CompFill(exprs, pd)

                tgtCell.label = Some(comp)
                fillCell.label = Some(fill)

              } else {

                logPane.debug("Coherence problem.")

              }

              // Now re-render and we should be good!!
              editor.editor.renderAll
              
            }
            case (_, None) => logPane.error("Base complex is not full.")
          }

        }
        case tl >> SBox(Some(tgt), cn) >> SDot(None) => {

          logPane.debug("Inverse lifting problem")

          val optTr = cn.map({
            case SDot(o) => o
            case _ => None
          })

          val exprTail = tl.traverseComplex(opt => opt)

          (extractDerivative(optTr), exprTail)  match {
            case (Xor.Right((deriv, addr)), Some(exprs)) => {

              val fillCell: EditorCell = bface.head.baseValue
              val srcCell: EditorCell = (for {
                t <- bface.tail
                n <- t.head.boxOption
                m <- n._2.elementAt(addr)
              } yield m.baseValue).get

              if (tl.dim == 0) {

                // Create a hom/app pair
                val hom = Hom(exprs, deriv, tgt)
                val app = App(exprs, deriv, tgt)
                
                srcCell.label = Some(hom)
                fillCell.label = Some(app)

                // Now re-render and we should be good!!
                editor.editor.renderAll

              } else if (tl.dim == 1) {
              } else {
              }

            }
            case (Xor.Left(msg), _) => logPane.error(msg)
            case (_, None) => logPane.error("Base complex is not full.")
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
