/**
  * FinsterCategory.scala - My own definition
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

class FinsterCategory(console: Logger) extends Theory(console) {

  //
  //  Expressions for omega categories
  //

  type ExprType = Expr
  type VarType = GenericVar

  sealed trait Expr {

    override def toString: String =
      this.pprint

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
      def isTgtUniversal: Boolean =
        e match {
          case ObjVar(_) => false
          case Var(_, _) => false
          case LeftComp(web, pd) => pd.forall(_.isTgtUniversal)
          case LeftFill(web, pd) => true
          case RightComp(web, deriv, tgt) => {

            // This is probably not the most general possibility. I only
            // allow the case where the lift is the root of its local tree
            // and all its children are target universal...

            val m = deriv.sh.traverse(_.traverse(expr =>
              verify(expr.isTgtUniversal,
                "Expression: " + expr.toString + " does not have target lifting."
              ))).flatMap(_ => succeed(()))

            (deriv.g.g.length == 0 && m.isRight)

          }
          case RightFill(web, deriv, tgt) => true
        }

      def isSrcUniversalAt(addr: SAddr): Boolean =
        e match {
          case ObjVar(_) => false
          case Var(_, _) => false
          case LeftComp(web, pd) => {

            // A left composite is source universal when there is a complete path
            // of source universal cells in its interior.

            def checkCtxt(g: List[(Expr, SDeriv[STree[Expr]])]): Except[Unit] =
              g match {
                case Nil => succeed(())
                case (e, d) :: h =>
                  for {
                    _ <- verify(e.isSrcUniversalAt(d.g.address),
                      "Expression: " + e.toString + " does not have source lifting at " + d.g.address)
                    _ <- checkCtxt(h)
                  } yield ()
              }

            (for {
              vAddr <- attempt(pd.horizToVertAddr(addr), "Failed to find incoming leaf.")
              z <- attempt(pd.seekTo(vAddr), "Failed seek for incoming leaf.")
              u <- checkCtxt(z.ctxt.g)
            } yield ()).isRight

          }
          case LeftFill(web, pd) => {

            // A LeftFill can be source universal when its boundary is fillable at
            // the given input.
            (for {
              z <- attempt(pd.seekTo(addr), "Failed to find cell in pasting diagram")
              nsh <- attempt(z.focus.nodeOption, "Selected input is not a node")
              _ <- isLiftable(SDeriv(nsh._2, z.ctxt))
            } yield ()).isRight

          }
          case RightComp(web, deriv, tgt) => {
            // Another kind of difficult case...
            false
          }
          case RightFill(web, deriv, tgt) => {
            // Here also its possible that you are missing some cases.
            // Should there not be some kind of default case when its
            // not at the obvious address...
            addr == deriv.g.address
          }

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
          case LeftComp(web, pd) => "LeftComp(" + pd.toList.map(_.toString).mkString(",") + ")"
          case LeftFill(web, pd) => "LeftFill(" + pd.toList.map(_.toString).mkString(",") + ")"
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

        def truncate(s: String): String =
          if (s.length > 10) (s.slice(0,10) + "...") else s

        e match {
          case o:ObjVar => CellRendering(text(truncate(o.toString)), colorSpec = VariableColorSpec)
          case v:Var => CellRendering(text(truncate(v.toString)), colorSpec = VariableColorSpec)
          case lc:LeftComp => CellRendering(text(truncate(lc.toString)), colorSpec = LeftCompColorSpec)
          case lf:LeftFill => CellRendering(text(truncate(lf.toString)), colorSpec = LeftFillColorSpec)
          case rc:RightComp => CellRendering(text(truncate(rc.toString)), colorSpec = RightCompColorSpec)
          case rf:RightFill => CellRendering(text(truncate(rf.toString)), colorSpec = RightFillColorSpec)
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
      stroke = "#dd97e8",
      strokeHovered = "#dd97e8",
      strokeSelected = "#dd97e8",
      edgeHovered = "#f19091"
    )

    object RightFillColorSpec extends ColorSpec(
      fill = "#FFFFFF",
      fillHovered = "#ffa8a8", // "#F3F4F5",
      fillSelected = "#ff7d7d",
      stroke = "#678bcf",
      strokeHovered = "#678bcf",
      strokeSelected = "#678bcf",
      edgeHovered = "#f19091"
    )


  }

  //
  //  Theory implementation
  // 

  def complexOf(e: ExprType): SComplex[ExprType] = 
    e.exprComplex

  def postulate(cmplx: SComplex[Option[Expr]], ident: String): Except[GenericVar] =
    cmplx match {
      case ObjectFrame() => succeed(ObjVar(ident))
      case CompleteFrame(frm) => succeed(Var(frm, ident))
      case _ => throwError("Incomplete Frame")
    }

  def isLiftable(deriv: SDeriv[Expr]): Except[Unit] = {

    def checkShell(sh: STree[STree[Expr]]): Except[Unit] =
      sh.traverse(_.traverse(expr =>
        verify(expr.isTgtUniversal,
          "Expression: " + expr.toString + " does not have target lifting."
        ))).flatMap(_ => succeed(()))

    def checkCtxt(g: List[(Expr, SDeriv[STree[Expr]])]): Except[Unit] =
      g match {
        case Nil => succeed(())
        case (e, d) :: h =>
          for {
            _ <- verify(e.isSrcUniversalAt(d.g.address),
              "Expression: " + e.toString + " does not have source lifting at " + d.g.address)
            _ <- checkShell(d.plug(SLeaf))
            _ <- checkCtxt(h)
          } yield ()
      }

    for {
      _ <- checkShell(deriv.sh)
      _ <- checkCtxt(deriv.g.g)
    } yield ()

  }

  def lift(cmplx: SComplex[Option[Expr]]): Except[Expr] = 
    cmplx match {
      case OutArrow(e) => throwError("No object lifts")
      case InArrow(e) => throwError("No object lifts")
      case OutNook(web, pd) => succeed(LeftFill(web, pd))
      case InNook(web, deriv, tgt) => {
        for {
          _ <- isLiftable(deriv)
        } yield RightFill(web, deriv, tgt)
      }
      case _ => throwError("Invalid lifting configuration")
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

  editor.onCellCtrlClick = (c: editor.StableCell) => {
    for {
      face <- c.face
    } { viewer.complex = Some(face) }
  }


}
