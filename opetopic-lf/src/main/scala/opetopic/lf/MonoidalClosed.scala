/**
  * MonoidalClosed.scala - Theory of a monoidal closed infinity category
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.lf

import org.scalajs.jquery._

import opetopic._
import opetopic.ui._
import opetopic.mtl._
import opetopic.js.ui._

class MonoidalClosed(console: Logger) extends Theory(console) {
  
  //
  //  Expressions for monoidal closed catgories
  //

  type ExprType = Expr
  type VarType = Var

  sealed trait Expr {

    override def toString: String =
      this.pprint

  }

  def complexOf(e: ExprType): SComplex[ExprType] = 
    e.exprComplex

  def postulate(cmplx: SComplex[Option[Expr]], ident: String): Except[Var] =
    cmplx match {
      case CompleteFrame(frm) => succeed(Var(frm, ident))
      case _ => throwError("Incomplete Frame")
    }

  def lift(cmplx: SComplex[Option[Expr]]): Except[Expr] =
    cmplx match {
      case OutArrow(_) => throwError("No object lifts")
      case InArrow(_) => throwError("No object lifts")
      case OutNook(web, pd) => succeed(LeftFill(web, pd))
      case InNook(web, deriv, tgt) => succeed(RightFill(web, deriv, tgt))
      case _ => throwError("Invalid lifting configuration")
    }

  case object Obj extends Expr
  case class Var(val frame: SComplex[Expr], val id: String) extends Expr
  case class LeftComp(val web: SComplex[Expr], val pd: STree[Expr]) extends Expr
  case class LeftFill(val web: SComplex[Expr], val pd: STree[Expr]) extends Expr
  case class RightComp(val web: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr
  case class RightFill(val web: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr

  object Expr {

    implicit class ExprOps[A](e: Expr) {

      def dim: Int =
        e match {
          case Obj => 0
          case Var(frame, id) => frame.dim + 1
          case LeftComp(web, pd) => web.dim + 1
          case LeftFill(web, pd) => web.dim + 2
          case RightComp(web, deriv, tgt) => web.dim + 1
          case RightFill(web, deriv, tgt) => web.dim + 2
        }

      def exprComplex: SComplex[Expr] =
        e match {
          case Obj => ||(SDot(Obj))
          case Var(frame, id) => frame >> SDot(e)
          case LeftComp(web, pd) => LeftFill(web, pd).exprComplex.target.get
          case LeftFill(web, pd) => web >> SBox(LeftComp(web, pd), pd.map(SDot(_))) >> SDot(e)
          case RightComp(web, deriv, tgt) => RightFill(web, deriv, tgt).exprComplex.face(FaceAddr(1, List(SDir(deriv.g.address)))).get
          case RightFill(web, deriv, tgt) => web >> SBox(tgt, deriv.plug(RightComp(web, deriv, tgt)).map(SDot(_))) >> SDot(e)
        }

      def pprint: String =
        e match {
          case Obj => "Obj"
          case Var(frame, id) => id
          case LeftComp(web, pd) =>
            web.dim match {
              case 0 =>
                if (pd.isLeaf) "Unit" else "(" + pd.toList.map(_.toString).mkString("x") + ")"
              case _ =>
                if (pd.isLeaf) "Id(" + web.head.baseValue.toString + ")"
                else "Comp(" + pd.toList.map(_.toString).mkString(",") + ")"
            }
          case LeftFill(web, pd) =>
            web.dim match {
              case 0 =>
                if (pd.isLeaf) "tt" else "Pair(" + pd.toList.map(_.toString).mkString(",") + ")"
              case _ =>
                if (pd.isLeaf) "Null(" + web.head.baseValue.toString + ")"
                else "Fill(" + pd.toList.map(_.toString).mkString(",") + ")"
            }
          case RightComp(web, deriv, tgt) =>
            web.dim match {
              case 0 => {
                val rights = deriv.sh.map(_.toList.map(_.toString)).toList.flatten.mkString("<-")
                val lefts = deriv.g.close(SLeaf).toList.map(_.toString).mkString("->")
                "(" + lefts + (if (lefts.length > 0) "->" else "") +
                tgt.toString + (if (rights.length > 0) "<-" else "") + rights + ")"
              }
              case 1 => {
                "Lam(" + tgt.toString + ")"
              }
              case _ => {
                "RightComp(" + tgt.toString + ")"
              }
            }
          case RightFill(web, deriv, tgt) =>
            web.dim match {
              case 0 => {
                val rights = deriv.sh.map(_.toList.map(_.toString)).toList.flatten.mkString("<-")
                val lefts = deriv.g.close(SLeaf).toList.map(_.toString).mkString("->")
                "App(" + lefts + ";" + tgt.toString + ";" + rights + ")"
              }
              case 1 => {
                "Beta(" + tgt.toString + ")"
              }
              case _ => {
                "RightFill(" + tgt.toString + ")"
              }
            }
        }

      def hasTargetLifting: Boolean =
        e match {
          case Obj => false
          case Var(frame, id) => frame.dim == 0
          case LeftComp(web, pd) => pd.forall(_.hasTargetLifting)
          case LeftFill(web, pd) => true
          case RightComp(web, deriv, tgt) => {
            (web.dim == 0) || (tgt.hasTargetLifting)
          }
          case RightFill(web, deriv, tgt) => false
        }

      def hasSourceLiftingAt(addr: SAddr): Boolean =
        e match {
          case Obj => false
          case Var(frame, id) => frame.dim == 0
          case LeftComp(web, pd) =>
            if (web.dim == 0) true
            else if (web.dim > 1) true
            else {

              def checkCtxt(g: List[(Expr, SDeriv[STree[Expr]])]): Except[Unit] =
                g match {
                  case Nil => succeed(())
                  case (e, d) :: h =>
                    for {
                      _ <- verify(e.hasSourceLiftingAt(d.g.address),
                        "Expression: " + e.toString + " does not have source lifting at " + d.g.address)
                      // Taking this out for now, but not sure if it should be included ...
                      // _ <- checkShell(d.plug(SLeaf))
                      _ <- checkCtxt(h)
                    } yield ()
                }

              (for {
                vAddr <- attempt(pd.horizToVertAddr(addr), "Faile to find incoming leaf.")
                z <- attempt(pd.seekTo(vAddr), "Failed seek for incoming leaf.")
                u <- checkCtxt(z.ctxt.g)
              } yield ()).isRight

            }
            
          case LeftFill(web, pd) => {
            // No in lowest dimension, and then we will
            // be in the Kan range.
            false
          }
          case RightComp(web, deriv, tgt) => web.dim == 0
          case RightFill(web, deriv, tgt) => addr == deriv.g.address
        }

    }

    //
    //  Semantic Rules
    //

    // Composites always exist
    def validateLeftLift(web: SComplex[Expr], pd: STree[Expr]): Except[Unit] =
      succeed(())

    def validateRightLift(web: SComplex[Expr], deriv: SDeriv[Expr], tgt: Expr): Except[Unit] = {

      def checkShell(sh: STree[STree[Expr]]): Except[Unit] =
        for {
          _ <- sh.traverse(_.traverse(expr =>
            verify(expr.hasTargetLifting,
              "Expression: " + expr.toString + " does not have target lifting."
            )))
        } yield ()

      def checkCtxt(g: List[(Expr, SDeriv[STree[Expr]])]): Except[Unit] =
        g match {
          case Nil => succeed(())
          case (e, d) :: h =>
            for {
              _ <- verify(e.hasSourceLiftingAt(d.g.address),
                "Expression: " + e.toString + " does not have source lifting at " + d.g.address)
              _ <- checkShell(d.plug(SLeaf))
              _ <- checkCtxt(h)
            } yield ()
        }

      if (web.dim >= 2)
        succeed(())  // Kan range ...
      else for {
        _ <- checkShell(deriv.sh)
        _ <- checkCtxt(deriv.g.g)
      } yield ()

    }

    //
    //  Renderable instance
    //

    implicit object ExprRenderable extends Renderable[Expr] {
      def render(f: UIFramework)(e: Expr): f.CellRendering = {

        import f._
        import isNumeric._

        implicit def intToUnit(i: Int) : Size =
          fromInt(i)

        e match {
          case Obj => CellRendering(spacer(Bounds(0, 0, 600, 600)), colorSpec = ObjectColorSpec)
          case v:Var => CellRendering(text(v.toString), colorSpec = VariableColorSpec)
          case lc:LeftComp => CellRendering(text(lc.toString), colorSpec = LeftCompColorSpec)
          case lf:LeftFill => CellRendering(text(lf.toString), colorSpec = LeftFillColorSpec)
          case rc:RightComp => CellRendering(text(rc.toString), colorSpec = RightCompColorSpec)
          case rf:RightFill => CellRendering(text(rf.toString), colorSpec = RightFillColorSpec)
        }

      }
    }

    object ObjectColorSpec extends ColorSpec(
      fill = "#dddddd",
      fillHovered = "#dddddd",
      fillSelected = "#cccccc",
      stroke = "#000000",
      strokeHovered = "#000000",
      strokeSelected = "#000000",
      edgeHovered = "#f19091"
    )

    object VariableColorSpec extends ColorSpec(
      fill = "#FFFFFF",
      fillHovered = "#ffa8a8", // "#F3F4F5",
      fillSelected = "#ff7d7d",
      stroke = "#ba5050",
      strokeHovered = "#ba5050",
      strokeSelected = "#ba5050",
      edgeHovered = "#f19091"
    )

    object LeftCompColorSpec extends ColorSpec(
      fill = "#FFFFFF",
      fillHovered = "#ffa8a8", // "#F3F4F5",
      fillSelected = "#ff7d7d",
      stroke = "#d6713e",
      strokeHovered = "#d6713e",
      strokeSelected = "#d6713e",
      edgeHovered = "#f19091"
    )

    object LeftFillColorSpec extends ColorSpec(
      fill = "#FFFFFF",
      fillHovered = "#ffa8a8", // "#F3F4F5",
      fillSelected = "#ff7d7d",
      stroke = "#3b8716",
      strokeHovered = "#3b8716",
      strokeSelected = "#3b8716",
      edgeHovered = "#f19091"
    )

    object RightCompColorSpec extends ColorSpec(
      fill = "#FFFFFF",
      fillHovered = "#ffa8a8", // "#F3F4F5",
      fillSelected = "#ff7d7d",
      stroke = "#303e80",
      strokeHovered = "#303e80",
      strokeSelected = "#303e80",
      edgeHovered = "#f19091"
    )

    object RightFillColorSpec extends ColorSpec(
      fill = "#FFFFFF",
      fillHovered = "#ffa8a8", // "#F3F4F5",
      fillSelected = "#ff7d7d",
      stroke = "#698030",
      strokeHovered = "#698030",
      strokeSelected = "#698030",
      edgeHovered = "#f19091"
    )


  }

  //
  //  Lift definitions
  //

  // override def onLift: Unit = 
  //   for {
  //     root <- editor.editor.selectionRoot
  //     face <- root.face
  //     bface <- root.boxFace
  //   } {

  //     face match {

  //       //
  //       // Left Lifting
  //       //

  //       case tl >> SBox(None, cn) >> SDot(None) => {

  //         val m = for {

  //           web <- attempt(tl.traverseComplex(o => o), "Incomplete base")
  //           pd <-  attempt(cn.traverse({
  //             case SDot(o) => o
  //             case _ => None
  //           }), "Error extracting pasting diagram.")
  //           _ <- Expr.validateLeftLift(web, pd)

  //         } yield (web, pd)

  //         m match {
  //           case Xor.Left(msg) => console.error(msg)
  //           case Xor.Right((web, pd)) => {

  //             console.debug("Finished left lift.")

  //             // The newly created expressions
  //             val comp = LeftComp(web, pd)
  //             val fill = LeftFill(web, pd)

  //             // The destination cells
  //             val compCell = bface.tail.get.head.baseValue
  //             val fillCell = bface.head.baseValue

  //             compCell.label = Some(comp)
  //             fillCell.label = Some(fill)
  //             editor.editor.renderAll

  //           }
  //         }

  //       }

  //       //
  //       //  Right Lifting
  //       //

  //       case tl >> SBox(Some(tgt), cn) >> SDot(None) => {

  //         val tr = cn.map({
  //           case SDot(o) => o
  //           case _ => None
  //         })

  //         tr.mapWithAddr({
  //           case (Some(e), _) => None
  //           case (_, addr) => Some(addr)
  //         }).toList.flatten match {
  //           case addr :: Nil => {

  //             val m = for {

  //               // Check that the base is full
  //               web <- attempt(tl.traverseComplex(o => o), "Incomplete frame")
                
  //               // Generate the appropriate derivative
  //               z <- attempt(tr.seekTo(addr), "Invalid address")
  //               noOptZip <- attempt(z.ctxt.close(SLeaf).map(_.get).seekTo(addr), "Internal error")
  //               noOptShell = z.focus.nodeOption.get._2.map(_.map(_.get))
  //               deriv = SDeriv(noOptShell, noOptZip.ctxt)

  //               // Validate the lift
  //               _ <- Expr.validateRightLift(web, deriv, tgt)

  //             } yield (web, deriv)

  //             m match {
  //               case Xor.Left(msg) => console.error(msg)
  //               case Xor.Right((web, deriv)) => {

  //                 // The newly created expressions
  //                 val comp = RightComp(web, deriv, tgt)
  //                 val fill = RightFill(web, deriv, tgt)

  //                 // The destination cells
  //                 val fillCell: EditorCell = bface.head.baseValue
  //                 val compCell: EditorCell = (for {
  //                   t <- bface.tail
  //                   n <- t.head.boxOption
  //                   m <- n._2.elementAt(addr)
  //                 } yield m.baseValue).get

  //                 // Set the new labels and re-render
  //                 compCell.label = Some(comp)
  //                 fillCell.label = Some(fill)
  //                 editor.editor.renderAll
                  
  //               }
  //             }
  //           }
  //           case _ => console.error("Inverse lifting problem does not have a unique empty source.")
  //         }
  //       }
  //       case _ => console.error("Malformed lifting condition")
  //     }

  //   }
  

  //
  //  UI Definitions
  //

  type EditorType = LambdaEditor
  type ViewerType = SimpleViewer[Option[Expr]]

  val defaultCardinal: SCardinal[Option[Expr]] =
    SCardinal(||(SDot(Some(Obj))))
  
  // A simple override of the extrusion commands to ensure we always
  // have an object in the lowest dimension
  class LambdaEditor extends SimpleCardinalEditor[Expr](defaultCardinal) {

    def selectionDim: Option[Int] =
      editor.selectionRoot.map(_.dim)

    override def handleKeyEvent(ev: JQueryEventObject): Unit = {
      ev.which match {
        case 101 => for { d <- selectionDim } {
          if (d == 0) {
            editor.extrudeSelectionWith(Some(Obj), None)
          } else editor.extrudeSelection
        }
        case 100 => editor.loopAtSelection
        case 115 => for { d <- selectionDim } {
          if (d == 0) {
            editor.sproutAtSelectionWith(Some(Obj), None)
          } else editor.sproutAtSelection
        }
        case _ => ()
      }
    }
    
  }

  // Editor and viewer instances
  val editor = new LambdaEditor
  val viewer = new SimpleViewer[Option[Expr]]

  editor.editor.onCellShiftClick = (c: editor.editor.CellType) => {
    for {
      face <- c.face
    } { viewer.complex = Some(face) }
  }
  
}
