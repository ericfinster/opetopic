/**
  * Expr.scala - Expressions for planar lambda calculus
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.lambda

import opetopic._
import opetopic.ui._
import opetopic.mtl._

sealed trait Expr {

  override def toString: String =
    this.pprint

}

case object Obj extends Expr
case class Var(val frame: SComplex[Expr], val id: String) extends Expr
case class LeftComp(val web: SComplex[Expr], val pd: STree[Expr]) extends Expr
case class LeftFill(val web: SComplex[Expr], val pd: STree[Expr]) extends Expr
case class RightComp(val web: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr
case class RightFill(val web: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr

object Expr {


  implicit class ExprOps[A](e: Expr) {

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
        case Obj => ""
        case Var(frame, id) => ""
        case LeftComp(web, pd) => ""
        case LeftFill(web, pd) => ""
        case RightComp(web, deriv, tgt) => ""
        case RightFill(web, deriv, tgt) => ""
      }

    // this match {
    //   case Var(id) => id
    //   case Prod(objs, exprs) =>
    //     "(" + exprs.toList.map(_.toString).mkString("x") + ")"
    //   case Pair(objs, exprs) =>
    //     "Pair(" + exprs.toList.map(_.toString).mkString(",") + ")"
    //   case Hom(objs, deriv, tgt) => {
    //     val rights = deriv.sh.map(_.toList.map(_.toString)).toList.flatten.mkString("<-")
    //     val lefts = deriv.g.close(SLeaf).toList.map(_.toString).mkString("->")
    //     "(" + lefts + (if (lefts.length > 0) "->" else "") +
    //       tgt.toString + (if (rights.length > 0) "<-" else "") + rights + ")"
    //   }
    //   case App(objs, deriv, tgt) => {
    //     val rights = deriv.sh.map(_.toList.map(_.toString)).toList.flatten.mkString("<-")
    //     val lefts = deriv.g.close(SLeaf).toList.map(_.toString).mkString("->")
    //     "App(" + lefts + ";" + tgt.toString + ";" + rights + ")"
    //   }
    //   case Comp(base, pd) => "Comp"
    //   case CompFill(base, pd) => "CompFill"
    //   case Lam(base, deriv, tgt) => "Lam"
    //   case LamFill(base, deriv, tgt) => "LamFill"
    //   case _ => "Unknown"
    // }


    // def hasTargetLifting(e: Expr): Boolean =
    //   e match {
    //     case Obj => false
    //     case Var(_) => false
    //     case Prod(_, _) => false
    //     case Pair(_, _) => true
    //     case Hom(_, _, _) => false
    //     case App(_, _, _) => false
    //     case Comp(_, pd) => pd.forall(hasTargetLifting(_))
    //     case CompFill(_, _) => true
    //   }

    // def hasSourceLifting(e: Expr, addr: SAddr): Boolean =
    //   e match {
    //     case Obj => false
    //     case Var(_) => false
    //     case Prod(_, _) => false
    //     case Pair(_, _) => false
    //     case Hom(_, _, _) => false
    //     // True if the source is the same as the position of the lift
    //     case App(_, d, _) => addr == d.g.address
    //     case Comp(_, _) => false
    //     case CompFill(_, _) => false
    //   }

  }

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
    fill = "#DDDDDD",
    fillHovered = "#DDDDDD",
    fillSelected = "#DDDDDD",
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
