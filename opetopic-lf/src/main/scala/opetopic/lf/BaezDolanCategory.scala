/**
  * BaezDolanCategory.scala - Theory of an omega category à la Baez and Dolan
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

class BaezDolanCategory(console: Logger) extends Theory(console) {

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
  case class Comp(val web: SComplex[Expr], val pd: STree[Expr]) extends Expr
  case class Fill(val web: SComplex[Expr], val pd: STree[Expr]) extends Expr
  case class Lift(val web: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr
  case class Connect(val web: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr

  object Expr {

    implicit class ExprOps[A](e: Expr) {

      def dim: Int =
        e match {
          case ObjVar(id) => 0 
          case Var(frame, id) => frame.dim + 1
          case Comp(web, pd) => web.dim + 1
          case Fill(web, pd) => web.dim + 2
          case Lift(web, deriv, tgt) => web.dim + 1
          case Connect(web, deriv, tgt) => web.dim + 2
        }

      // Assumes the expression is well-formed
      def isTgtUniversal: Boolean =
        e match {
          case ObjVar(_) => false
          case Var(_, _) => false
          case Comp(web, pd) => pd.forall(_.isTgtUniversal)  // Is this a good idea?
          case Fill(web, pd) => true
          case Lift(web, deriv, tgt) => false  // Yikes
          case Connect(web, deriv, tgt) => true
        }

      def isSrcUniversalAt(addr: SAddr): Boolean =
        e match {
          case ObjVar(_) => false
          case Var(_, _) => false
          case Comp(web, pd) => false 
          case Fill(web, pd) => false
          case Lift(web, deriv, tgt) => false  // Yikes
          case Connect(web, deriv, tgt) => addr == deriv.g.address
        }

      def exprComplex: SComplex[Expr] =
        e match {
          case ObjVar(id) => ||(SDot(e))
          case Var(frame, id) => frame >> SDot(e)
          case Comp(web, pd) => Fill(web, pd).exprComplex.target.get
          case Fill(web, pd) => web >> SBox(Comp(web, pd), pd.map(SDot(_))) >> SDot(e)
          case Lift(web, deriv, tgt) => Connect(web, deriv, tgt).exprComplex.face(FaceAddr(1, List(SDir(deriv.g.address)))).get
          case Connect(web, deriv, tgt) => web >> SBox(tgt, deriv.plug(Lift(web, deriv, tgt)).map(SDot(_))) >> SDot(e)
        }

      def pprint: String =
        e match {
          case ObjVar(id) => id
          case Var(frame, id) => id
          case Comp(web, pd) => "Comp(" + pd.toList.map(_.toString).mkString(",") + ")"
          case Fill(web, pd) => "Fill(" + pd.toList.map(_.toString).mkString(",") + ")"
          case Lift(web, deriv, tgt) => "Lift(" + tgt.toString + ")"
          case Connect(web, deriv, tgt) => "Connect(" + tgt.toString + ")"
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
          case lc:Comp => CellRendering(text(lc.toString), colorSpec = CompColorSpec)
          case lf:Fill => CellRendering(text(lf.toString), colorSpec = FillColorSpec)
          case rc:Lift => CellRendering(text(rc.toString), colorSpec = LiftColorSpec)
          case rf:Connect => CellRendering(text(rf.toString), colorSpec = ConnectColorSpec)
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

    object LiftColorSpec extends ColorSpec(
      fill = "#FFFFFF",
      fillHovered = "#ffa8a8", // "#F3F4F5",
      fillSelected = "#ff7d7d",
      stroke = "#303e80",
      strokeHovered = "#303e80",
      strokeSelected = "#303e80",
      edgeHovered = "#f19091"
    )

    object ConnectColorSpec extends ColorSpec(
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


  object TargetPrimeNook {

    def unapply(cmplx: SComplex[Option[Expr]]): Option[(SComplex[Expr], Expr, Expr, SDeriv[Expr], SAddr)] =
      cmplx match {
        case tl >> SBox(Some(tgtExpr), SNode(SDot(None), sh)) >> SDot(None) => {

          var exprs = List[(Expr, SAddr)]()

          for {
            web <- tl.traverseComplex(o => o)
            nsh <- sh.traverseWithAddr({
              case (SLeaf, _) => Some(SLeaf)
              case (SNode(SDot(Some(srcExpr)), ssh), addr) =>
                if (ssh.forall(_.isLeaf)) {
                  exprs = (srcExpr, addr) :: exprs
                  Some(SNode(srcExpr, ssh.map(_ => SLeaf)))
                } else None
            })
            r <- exprs match {
              case (srcExpr, addr) :: Nil => {
                val deriv = SDeriv(nsh, SCtxt(Nil))
                Some(web, tgtExpr, srcExpr, deriv, addr)
              }
              case _ => None
            }
          } yield r

        }
        case _ => None
      }

  }

  object SourcePrimeNook {

    def unapply(cmplx: SComplex[Option[Expr]]): Option[(SComplex[Expr], Expr, Expr, SDeriv[Expr], SAddr)] =
      cmplx match {
        case tl >> SBox(Some(tgtExpr), SNode(SDot(Some(srcExpr)), sh)) >> SDot(None) => {

          var addrs = List[SAddr]()

          for {
            web <- tl.traverseComplex(o => o)
            nsh <- sh.traverseWithAddr({
              case (SLeaf, _) => Some(SLeaf)
              case (SNode(SDot(None), ssh), addr) =>
                if (ssh.forall(_.isLeaf)) {
                  addrs = addr :: addrs
                  // We put the source expression in the wrong place
                  // temporarily to fixup types.  We'll throw it away below
                  Some(SNode(srcExpr, ssh.map(_ => SLeaf)))
                } else None
            })
            r <- addrs match {
              case addr :: Nil => {
                for {
                  z <- SNode(srcExpr, nsh).seekTo(SDir(addr) :: Nil)
                  (_, osh) <- z.focus.nodeOption
                } yield {
                  val deriv = SDeriv[Expr](osh, z.ctxt)
                  (web, tgtExpr, srcExpr, deriv, addr)
                }
              }
              case _ => None
            }
          } yield r

        }
        case _ => None
      }

  }

  def lift(cmplx: SComplex[Option[Expr]]): Except[Expr] = 
    cmplx match {
      case OutArrow(e) => throwError("No object lifts")
      case InArrow(e) => throwError("No object lifts")
      case OutNook(web, pd) => succeed(Fill(web, pd))
      case TargetPrimeNook(web, tgtExpr, srcExpr, deriv, addr) => {
        if (srcExpr.isTgtUniversal) {
          succeed(Connect(web, deriv, tgtExpr)) 
        } else throwError("Expression " + srcExpr.toString + " is not target universal")
      }
      case SourcePrimeNook(web, tgtExpr, srcExpr, deriv, addr) => {
        if (srcExpr.isSrcUniversalAt(addr)) {
          succeed(Connect(web, deriv, tgtExpr)) 
        } else throwError("Expression " + srcExpr.toString + " is not source universal at " + addr.toString)
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

  editor.onCellShiftClick = (c: editor.StableCell) => {
    for {
      face <- c.face
    } { viewer.complex = Some(face) }
  }


}
