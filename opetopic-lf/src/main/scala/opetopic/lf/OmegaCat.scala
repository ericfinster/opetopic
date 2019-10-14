/**
  * OmegaCat.scala - Theory of an omega category
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

class OmegaCat(console: Logger) extends Theory(console) {

  //
  //  Expressions for monoidal closed catgories
  //

  type ExprType = Expr
  type VarType = GenericVar

  sealed trait Expr {

    override def toString: String =
      this.pprint

  }

  def complexOf(e: ExprType): SComplex[ExprType] = 
    e.exprComplex

  def postulate(cmplx: SComplex[Option[Expr]], ident: String): Except[GenericVar] =
    cmplx match {
      case ObjectFrame() => succeed(ObjVar(ident))
      case CompleteFrame(frm) => succeed(Var(frm, ident))
      case _ => throwError("Incomplete Frame")
    }

  def isExposedNook(cmplx: SComplex[Option[Expr]]): Except[Unit] =
    cmplx match {
      case OutArrow(e) => succeed(())
      case InArrow(e) => succeed(())
      case OutNook(_, _) => succeed(())
      case InNook(web, deriv, tgt) => {

        def checkShell(sh: STree[STree[Expr]]): Except[Unit] =
          for {
            _ <- sh.traverse(_.traverse(expr =>
              verify(expr.isUniversal,
                "Expression: " + expr.toString + " is not universal"
              )))
          } yield ()

        def checkCtxt(g: List[(Expr, SDeriv[STree[Expr]])]): Except[Unit] =
          g match {
            case Nil => succeed(())
            case (e, d) :: h =>
              for {
                frm <- attempt(e.exprComplex.mapComplex[Option[Expr]](Some(_)).withValueAt(FaceAddr(0, Nil), None), "Error creating frame")
                inNook <- attempt(frm.withValueAt(FaceAddr(1, List(SDir(d.g.address))), None), "Error extracting inNook")
                _ <- isExposedNook(inNook)
                _ <- checkShell(d.plug(SLeaf))
                _ <- checkCtxt(h)
              } yield ()
          }

        def isExposed(d: SDeriv[Expr]): Except[Unit] =
          for {
            _ <- checkShell(deriv.sh)
            _ <- checkCtxt(deriv.g.g)
          } yield ()

        // Should start the chain of inductive hypotheses...
        isExposed(deriv)
        
      }
    }

  def lift(cmplx: SComplex[Option[Expr]]): Except[Expr] =
    cmplx match {
      case OutArrow(e) => throwError("No object lifts")
      case InArrow(e) => throwError("No object lifts")
      case OutNook(web, pd) => succeed(LeftFill(web, pd))
      case InNook(web, deriv, tgt) =>
        isExposedNook(cmplx).flatMap(_ =>
          succeed(RightFill(web, deriv, tgt)))
      case _ => throwError("Invalid lifting configuration")
    }

  trait GenericVar extends Expr
  case class ObjVar(val id: String) extends GenericVar
  case class Var(val frame: SComplex[Expr], val id: String) extends GenericVar
  case class LeftComp(val web: SComplex[Expr], val pd: STree[Expr]) extends Expr
  case class LeftFill(val web: SComplex[Expr], val pd: STree[Expr]) extends Expr
  case class RightComp(val web: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr
  case class RightFill(val web: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr

  object Expr {

    implicit class ExprOps[A](e: Expr) {

      def dim: Int =
        e match {
          case ObjVar(id) => 0 
          case Var(frame, id) => frame.dim + 1
          case LeftComp(web, pd) => web.dim + 1
          case LeftFill(web, pd) => web.dim + 2
          case RightComp(web, deriv, tgt) => web.dim + 1
          case RightFill(web, deriv, tgt) => web.dim + 2
        }

      // Assumes the expression is well-formed
      def isUniversal: Boolean =
        e match {
          case ObjVar(_) => false
          case Var(_, _) => false
          case LeftComp(_, pd) => pd.forall(_.isUniversal)
          case LeftFill(_, _) => true
          case RightComp(_, _, tgt) => tgt.isUniversal
          case RightFill(_, _, _) => true
        }

      def exprComplex: SComplex[Expr] =
        e match {
          case ObjVar(id) => ||(SDot(e))
          case Var(frame, id) => frame >> SDot(e)
          case LeftComp(web, pd) => LeftFill(web, pd).exprComplex.target.get
          case LeftFill(web, pd) => web >> SBox(LeftComp(web, pd), pd.map(SDot(_))) >> SDot(e)
          case RightComp(web, deriv, tgt) => RightFill(web, deriv, tgt).exprComplex.face(FaceAddr(1, List(SDir(deriv.g.address)))).get
          case RightFill(web, deriv, tgt) => web >> SBox(tgt, deriv.plug(RightComp(web, deriv, tgt)).map(SDot(_))) >> SDot(e)
        }

      def pprint: String =
        e match {
          case ObjVar(id) => id
          case Var(frame, id) => id
          case LeftComp(web, pd) => "Comp(" + pd.toList.map(_.toString).mkString(",") + ")"
          case LeftFill(web, pd) => "Fill(" + pd.toList.map(_.toString).mkString(",") + ")"
          case RightComp(web, deriv, tgt) => "RightComp(" + tgt.toString + ")"
          case RightFill(web, deriv, tgt) => "RightFill(" + tgt.toString + ")"
        }

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
          case o:ObjVar => CellRendering(text(o.toString), colorSpec = VariableColorSpec)
          case v:Var => CellRendering(text(v.toString), colorSpec = VariableColorSpec)
          case lc:LeftComp => CellRendering(text(lc.toString), colorSpec = LeftCompColorSpec)
          case lf:LeftFill => CellRendering(text(lf.toString), colorSpec = LeftFillColorSpec)
          case rc:RightComp => CellRendering(text(rc.toString), colorSpec = RightCompColorSpec)
          case rf:RightFill => CellRendering(text(rf.toString), colorSpec = RightFillColorSpec)
        }

      }
    }

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
  //  UI Definitions
  //

  type EditorType = TabbedCardinalEditor[Expr]
  type ViewerType = SimpleViewer[Option[Expr]]

  val defaultCardinal: SCardinal[Option[Expr]] =
    SCardinal()

  // Editor and viewer instances
  val editor = new TabbedCardinalEditor[Expr]
  val viewer = new SimpleViewer[Option[Expr]]

  editor.onCellShiftClick = (c: editor.StableCell) => {
    for {
      face <- c.face
    } { viewer.complex = Some(face) }
  }


}
