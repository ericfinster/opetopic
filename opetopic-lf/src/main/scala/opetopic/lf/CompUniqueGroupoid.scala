/**
  * CompUniqueGroupoid.scala - A groupoid structure defined by contractibility of composites
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

class CompUniqueGroupoid(console: Logger) extends Theory(console) {

  //
  //  Expressions for groupoids
  //

  type ExprType = Expr
  type VarType = GenericVar

  sealed trait Expr {

    override def toString: String =
      this.pprint

  }

  type VShell = Shell[SNesting[Expr]]
  type HShell = Shell[STree[SNesting[Expr]]]

  sealed trait GenericVar extends Expr
  case class ObjVar(val id: String) extends GenericVar
  case class Var(val frame: SComplex[Expr], val id: String) extends GenericVar
  case class Comp(val web: SComplex[Expr], val pd: STree[Expr]) extends Expr
  case class Fill(val web: SComplex[Expr], val pd: STree[Expr]) extends Expr
  case class Unique(val web: SComplex[Expr], val src: Expr, val tgt: Expr, val vsh: VShell, val hsh : HShell) extends Expr
  case class Compat(val web: SComplex[Expr], val src: Expr, val tgt: Expr, val vsh: VShell, val hsh : HShell) extends Expr

  object Expr {

    implicit class ExprOps[A](e: Expr) {

      def dim: Int =
        e match {
          case ObjVar(id) => 0 
          case Var(frame, id) => frame.dim + 1
          case Comp(web, pd) => web.dim + 1
          case Fill(web, pd) => web.dim + 2
          case Unique(web, src, tgt, vsh, hsh) => web.dim + 1
          case Compat(web, src, tgt, vsh, hsh) => web.dim + 2
        }

      def exprComplex: SComplex[Expr] =
        e match {
          case ObjVar(id) => ||(SDot(e))
          case Var(frame, id) => frame >> SDot(e)
          case Comp(web, pd) => Fill(web, pd).exprComplex.target.get
          case Fill(web, pd) => web >> SBox(Comp(web, pd), pd.map(SDot(_))) >> SDot(e)
          case Unique(web, src, tgt, vsh, hsh) =>
            Compat(web,src,tgt,vsh,hsh).exprComplex.face(FaceAddr(1,List(SDir(Nil)))).get
          case Compat(web, src, tgt, vsh, hsh) =>
            web >> SBox(tgt, SNode(SDot(Unique(web,src,tgt,vsh,hsh)), SNode(SNode(SDot(src),vsh),hsh))) >> SDot(e)
        }

      def pprint: String =
        e match {
          case ObjVar(id) => id
          case Var(frame, id) => id
          case Comp(web, pd) => "Comp(" + pd.toList.map(_.toString).mkString(",") + ")"
          case Fill(web, pd) => "Fill(" + pd.toList.map(_.toString).mkString(",") + ")"
          case Unique(web, src, tgt, vsh, hsh) => "Unique(" + src.toString + ", " + tgt.toString + ")"
          case Compat(web, src, tgt, vsh, hsh) => "Compat(" + src.toString + ", " + tgt.toString + ")"
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
          case lc:Comp => CellRendering(text(truncate(lc.toString)), colorSpec = CompColorSpec)
          case lf:Fill => CellRendering(text(truncate(lf.toString)), colorSpec = FillColorSpec)
          case rc:Unique => CellRendering(text(truncate(rc.toString)), colorSpec = UniqueColorSpec)
          case rf:Compat => CellRendering(text(truncate(rf.toString)), colorSpec = CompatColorSpec)
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

    object CompColorSpec extends ColorSpec(
      fill = "#FFFFFF",
      fillHovered = "#ffa8a8", // "#F3F4F5",
      fillSelected = "#ff7d7d",
      stroke = "#d6713e",
      strokeHovered = "#d6713e",
      strokeSelected = "#d6713e",
      edgeHovered = "#f19091"
    )

    object FillColorSpec extends ColorSpec(
      fill = "#FFFFFF",
      fillHovered = "#ffa8a8", // "#F3F4F5",
      fillSelected = "#ff7d7d",
      stroke = "#3b8716",
      strokeHovered = "#3b8716",
      strokeSelected = "#3b8716",
      edgeHovered = "#f19091"
    )

    object UniqueColorSpec extends ColorSpec(
      fill = "#FFFFFF",
      fillHovered = "#ffa8a8", // "#F3F4F5",
      fillSelected = "#ff7d7d",
      stroke = "#dd97e8",
      strokeHovered = "#dd97e8",
      strokeSelected = "#dd97e8",
      edgeHovered = "#f19091"
    )

    object CompatColorSpec extends ColorSpec(
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

  object UnaryComp {
    def unapply(c: SComplex[Option[Expr]]): Option[(SComplex[Expr], Expr)] =
      c match {
        case tl >> SBox(None, SNode(SDot(Some(e)), sh)) >> SDot(None) =>
          for {
            web <- tl.traverseComplex(o => o)
            // Shell should consist entirely of leaves
            _ <- if (sh.forall(_.isLeaf)) Some(()) else None
          } yield (web, e)
        case _ => None
      }
  }

  object AssertUnique {
    def unapply(c: SComplex[Option[Expr]]): Option[(SComplex[Expr], Expr, Expr, VShell, HShell)] =
      c match {
        case tl >> SBox(Some(tgt), SNode(SDot(None), SNode(SNode(SDot(Some(src)),vsh),hsh))) >> SDot(None) =>
          for {
            web <- tl.traverseComplex(o => o)
            // Both hsh and vsh should now be only leaves...
            _ <- if (vsh.forall(_.isLeaf)) Some(()) else None
            _ <- if (hsh.forall(_.isLeaf)) Some(()) else None
            // so that this is now safe.
            vshExpr = vsh.asShell[SNesting[Expr]]
            hshExpr = hsh.asShell[STree[SNesting[Expr]]]
          } yield (web, src, tgt, vshExpr, hshExpr)
        case _ => None
      }
  }

  def lift(cmplx: SComplex[Option[Expr]]): Except[Expr] =
    cmplx match {
      case OutArrow(e) => {
        // return comp of the identity on e
        succeed(Comp(||(SDot(e)), SLeaf))
      }
      case UnaryComp(web, e) => {
        // again, do identity on e
        succeed(Comp(web >> SDot(e), SLeaf))
      }
      case OutNook(web, pd) => {
        // Normal composition
        succeed(Fill(web, pd))
      }
      case AssertUnique(web, src, tgt, vsh, hsh) => {
        succeed(Compat(web, src, tgt, vsh, hsh))
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
