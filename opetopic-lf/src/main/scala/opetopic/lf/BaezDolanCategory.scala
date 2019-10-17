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

  // For playing with adjoints
  case class Hom(val web: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr
  case class App(val web: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr

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
          case Hom(web, deriv, tgt) => web.dim + 1
          case App(web, deriv, tgt) => web.dim + 2
        }

      // Assumes the expression is well-formed
      def isTgtUniversal: Boolean =
        e match {
          case ObjVar(_) => false
          case Var(_, _) => false
          // Note that this is actually *necessary* to prove the unit law is invertible ...
          case Comp(web, pd) => pd.forall(_.isTgtUniversal)  
          case Fill(web, pd) => true
          case Lift(web, deriv, tgt) => {

            // Here, I'm not quite sure to understand.
            // But one case I *do* think I understand is just when it
            // is a lift at the root of the internal tree.  In this case,
            // I feel that the only condition should be that all the others
            // are and the target is.

            false  // Yikes

          }
          case Connect(web, deriv, tgt) => true
          case Hom(web, deriv, tgt) => false
          case App(web, deriv, tgt) => true
        }

      def isSrcUniversalAt(addr: SAddr): Boolean =
        e match {
          case ObjVar(_) => false
          case Var(_, _) => false
          case Comp(web, pd) => {

              def checkCtxt(g: List[(Expr, SDeriv[STree[Expr]])]): Except[Unit] =
                g match {
                  case Nil => succeed(())
                  case (e, d) :: h =>
                    for {
                      _ <- verify(e.isSrcUniversalAt(d.g.address),
                        "Expression: " + e.toString + " is not source universal at " + d.g.address)
                      // Taking this out for now, but not sure if it should be included ...
                      // _ <- checkShell(d.plug(SLeaf))
                      _ <- checkCtxt(h)
                    } yield ()
                }

              (for {
                vAddr <- attempt(pd.horizToVertAddr(addr), "Failed to find incoming leaf.")
                z <- attempt(pd.seekTo(vAddr), "Failed seek for incoming leaf.")
                u <- checkCtxt(z.ctxt.g)
              } yield ()).isRight

          }
          case Fill(web, pd) => false
          case Lift(web, deriv, tgt) => {

            false  // Yikes..

          }
          case Connect(web, deriv, tgt) => addr == deriv.g.address
          case Hom(web, deriv, tgt) => {
            // Hmmm.  What about this?
            false
          }
          case App(web, deriv, tgt) => addr == deriv.g.address
        }

      def exprComplex: SComplex[Expr] =
        e match {
          case ObjVar(id) => ||(SDot(e))
          case Var(frame, id) => frame >> SDot(e)
          case Comp(web, pd) => Fill(web, pd).exprComplex.target.get
          case Fill(web, pd) => web >> SBox(Comp(web, pd), pd.map(SDot(_))) >> SDot(e)
          case Lift(web, deriv, tgt) => Connect(web, deriv, tgt).exprComplex.face(FaceAddr(1, List(SDir(deriv.g.address)))).get
          case Connect(web, deriv, tgt) => web >> SBox(tgt, deriv.plug(Lift(web, deriv, tgt)).map(SDot(_))) >> SDot(e)
          case Hom(web, deriv, tgt) => App(web, deriv, tgt).exprComplex.face(FaceAddr(1, List(SDir(deriv.g.address)))).get
          case App(web, deriv, tgt) => web >> SBox(tgt, deriv.plug(Hom(web, deriv, tgt)).map(SDot(_))) >> SDot(e)
        }

      def pprint: String =
        e match {
          case ObjVar(id) => id
          case Var(frame, id) => id
          case Comp(web, pd) => "Comp(" + pd.toList.map(_.toString).mkString(",") + ")"
          case Fill(web, pd) => "Fill(" + pd.toList.map(_.toString).mkString(",") + ")"
          case Lift(web, deriv, tgt) => "Lift(" + tgt.toString + ")"
          case Connect(web, deriv, tgt) => "Connect(" + tgt.toString + ")"
          case Hom(web, deriv, tgt) => {
            val rights = deriv.sh.map(_.toList.map(_.toString)).toList.flatten.mkString("←")
            val lefts = deriv.g.close(SLeaf).toList.map(_.toString).mkString("→")
            "(" + lefts + (if (lefts.length > 0) "→" else "") +
            tgt.toString + (if (rights.length > 0) "←" else "") + rights + ")"
          }
          case App(web, deriv, tgt) => {
            val rights = deriv.sh.map(_.toList.map(_.toString)).toList.flatten.mkString("←")
            val lefts = deriv.g.close(SLeaf).toList.map(_.toString).mkString("→")
            "App(" + lefts + ";" + tgt.toString + ";" + rights + ")"
          }
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
          case rc:Lift => CellRendering(text(truncate(rc.toString)), colorSpec = LiftColorSpec)
          case rf:Connect => CellRendering(text(truncate(rf.toString)), colorSpec = ConnectColorSpec)
          case h:Hom => CellRendering(text(truncate(h.toString)), colorSpec = LiftColorSpec)
          case a:App => CellRendering(text(truncate(a.toString)), colorSpec = ConnectColorSpec)
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
      stroke = "#dd97e8",
      strokeHovered = "#dd97e8",
      strokeSelected = "#dd97e8",
      edgeHovered = "#f19091"
    )

    object ConnectColorSpec extends ColorSpec(
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
        } else if (web.dim == 0) {
          succeed(App(web, deriv, tgtExpr))
        } else throwError("Expression " + srcExpr.toString + " is not target universal")
      }
      case SourcePrimeNook(web, tgtExpr, srcExpr, deriv, addr) => {
        if (srcExpr.isSrcUniversalAt(addr)) {
          succeed(Connect(web, deriv, tgtExpr)) 
        } else if (web.dim == 0) {
          succeed(App(web, deriv, tgtExpr))
        } else throwError("Expression " + srcExpr.toString + " is not source universal at " + addr.toString)
      }
      case InNook(web, deriv, tgt) => {
        if (web.dim == 0) {
          succeed(App(web, deriv, tgt))
        } else throwError("No right lifts in this dimension.")
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
