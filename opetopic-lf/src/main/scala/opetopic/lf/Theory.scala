/**
  * Theory.scala - Base class for opetopic theories
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.lf

import scala.collection.mutable.Map

import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic._
import opetopic.mtl._
import opetopic.js.ui._

abstract class Theory(val console: Logger) {

  type ExprType
  type VarType <: ExprType
  type DefType <: ExprType

  type EditorType <: SimpleCardinalEditor[ExprType]
  type ViewerType <: SimpleViewer[Option[ExprType]]

  val editor: EditorType
  val viewer: ViewerType

  val defaultCardinal: SCardinal[Option[ExprType]]

  type EditorCell = editor.StableCell
  type ViewerCell = viewer.CellType

  val environment: Map[String, ExprType] = Map()

  def initialize: Unit = {
    editor.initialize
  }

  def showExpr(expr: ExprType): Unit = {
    val cmplx: SComplex[Option[ExprType]] =
      complexOf(expr).map(Some(_))
    viewer.complex = Some(cmplx)
  }

  //
  //  Lift Destructors
  //

  object ObjectFrame {
    def unapply(c: SComplex[Option[ExprType]]): Boolean =
      c match {
        case ||(SDot(None)) => true
        case _ => false
      }
  }

  object CompleteFrame {
    def unapply(c: SComplex[Option[ExprType]]): Option[SComplex[ExprType]] =
      c match {
        case ||(SBox(Some(t), SNode(SDot(Some(s)), SLeaf))) >> SDot(None) =>
          Some(||(SBox(t, SNode(SDot(s), SLeaf))))
        case tl >> SBox(Some(tgt), cn) >> SDot(None) =>
          for {
            base <- tl.traverseComplex(o => o)
            pd <- cn.traverse({
              case SDot(Some(e)) => Some(SDot(e))
              case _ => None
            })
          } yield base >> SBox(tgt, pd)
        case _ => None
      }
  }

  object OutArrow {
    def unapply(c: SComplex[Option[ExprType]]): Option[ExprType] =
      c match {
        case ||(SBox(None, SNode(SDot(Some(e)), SLeaf))) >> SDot(None) => Some(e)
        case _ => None
      }
  }

  object OutNook {
    def unapply(c: SComplex[Option[ExprType]]): Option[(SComplex[ExprType], STree[ExprType])] =
      c match {
        case tl >> SBox(None, cn) >> SDot(None) =>
          for {
            web <- tl.traverseComplex(o => o)
            pd <-  cn.traverse({
              case SDot(o) => o
              case _ => None
            })
          } yield (web, pd)
        case _ => None
      }
  }

  object InArrow {
    def unapply(c: SComplex[Option[ExprType]]): Option[ExprType] =
      c match {
        case ||(SBox(Some(e), SNode(SDot(None), SLeaf))) >> SDot(None) => Some(e)
        case _ => None
      }
  }

  object InNook {
    def unapply(c: SComplex[Option[ExprType]]): Option[(SComplex[ExprType], SDeriv[ExprType], ExprType)] =
      c match {
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

              for {
                web <- tl.traverseComplex(o => o)
                z <- tr.seekTo(addr)
                noOptZip <- z.ctxt.close(SLeaf).map(_.get).seekTo(addr)
                noOptShell = z.focus.nodeOption.get._2.map(_.map(_.get))
                deriv = SDeriv(noOptShell, noOptZip.ctxt)
              } yield (web, deriv, tgt)

            }
            case _ => None
          }
          
        }
        case _ => None
      }
  }
  
  //
  //  Logical structure
  //

  def complexOf(e: ExprType): SComplex[ExprType]

  def postulate(frm: SComplex[Option[ExprType]], ident: String): Except[VarType]
  def lift(cmplx: SComplex[Option[ExprType]]): Except[ExprType]

  //
  //  Attempt to past the active viewer cell to the editor
  //

  def pasteFromViewer: Except[Unit] =
    for {
      cell <- attempt(viewer.complex, "No cell in viewer")
      u <- paste(cell)
    } yield u

  def paste(cell: SComplex[Option[ExprType]]): Except[Unit] = 
    for {
      root <- attempt(editor.editor.selectionRoot, "Nothing selected")
      face <- attempt(root.boxFace, "Error geting face")
      pc <- attempt(face.matchWith(cell), "Incompatible shape")
      cmplx <- pc.traverseComplex[Except, (EditorCell, ExprType)]({
        case (c, exprOpt) => {

          (c.label, exprOpt) match {
            case (Some(e), Some(f)) => {
              if (e.equals(f)) {
                succeed((c, e))
              } else {
                throwError("Incompatible expressions: " + e.toString + " =/= " + f.toString)
              }
            }
            case (None, Some(f)) => succeed((c, f))
            case (Some(e), None) => succeed((c, e))
            case (None, None) => throwError("Incomplete expression.")
          }

        }
      })

    } yield {

      // Update the complex
      cmplx.foreach({
        case (c, e) => { c.label = Some(e) }
      })

      // and re-render
      editor.editor.renderAll

    }

  //
  //  Define an expression
  //

  def onDefineExpr: Unit =
    for {
      root <- editor.editor.selectionRoot
      face <- root.face
    } {

      val exprs: List[Option[ExprType]] = face.toList

      if (exprs.forall(_.isDefined)) {
        console.debug("Ready to save cell")

        val inputName = jQuery("#expr-name-input").value.asInstanceOf[String]
        val expr = face.head.baseValue.get

        // This allows to quickly define a cell by it's summary
        val exprName =
          if (inputName == "")
            expr.toString
          else inputName
        
        val uiElement = a(cls := "item",
          onclick := { () => showExpr(expr) })(exprName).render
        
        jQuery("#editor-left-sidebar").append(uiElement)
        environment += (exprName -> expr)

        console.ok("Defined expression " + exprName)

      } else {
        console.error("Selected cell is not full.")
      }
    }


  def doPostulate: Except[Unit] =
    for {
      root <- attempt(editor.editor.selectionRoot, "Nothing selected")
      face <- attempt(root.face, "Error extracting face")
      bface <- attempt(root.boxFace, "Error extracting face")
      ident = jQuery("#postulate-input").value.asInstanceOf[String]
      expr <- postulate(face, ident)
    } yield {

      val uiElement = a(cls := "item",
        onclick := { () => showExpr(expr) })(ident).render
      jQuery("#editor-left-sidebar").append(uiElement)

      bface.head.baseValue.label = Some(expr)

      editor.editor.renderAll

      console.ok("Created new postulated cell " + ident)

    }

  def doLift: Except[Unit] =
    for {
      root <- attempt(editor.editor.selectionRoot, "Nothing selected")
      face <- attempt(root.face, "Error extracting face")
      expr <- lift(face)
      // Yeah, this is a bit wasteful as we already know the expression
      // with be well formed and doing it this way rechecks the well-formedness
      // of the boundary ... you could have an unsafe paste method ....
      u <- paste(complexOf(expr).mapComplex(Some(_)))
    } yield u

}
