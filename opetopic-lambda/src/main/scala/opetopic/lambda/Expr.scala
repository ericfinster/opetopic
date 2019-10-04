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
    this match {
      case Var(id) => id
      case Prod(objs, exprs) =>
        "(" + exprs.toList.map(_.toString).mkString("x") + ")"
      case Pair(objs, exprs) =>
        "Pair(" + exprs.toList.map(_.toString).mkString(",") + ")"
      case Hom(objs, deriv, tgt) => {
        val rights = deriv.sh.map(_.toList.map(_.toString)).toList.flatten.mkString("<-")
        val lefts = deriv.g.close(SLeaf).toList.map(_.toString).mkString("->")
        "(" + lefts + (if (lefts.length > 0) "->" else "") +
          tgt.toString + (if (rights.length > 0) "<-" else "") + rights + ")"
      }
      case App(objs, deriv, tgt) => {
        val rights = deriv.sh.map(_.toList.map(_.toString)).toList.flatten.mkString("<-")
        val lefts = deriv.g.close(SLeaf).toList.map(_.toString).mkString("->")
        "App(" + lefts + ";" + tgt.toString + ";" + rights + ")"
      }
      case Comp(base, pd) => "Comp"
      case CompFill(base, pd) => "CompFill"
      case _ => "Unknown"
    }

}

// Dim 0 
case object Obj extends Expr

// Dim 1
case class Var(val id: String) extends Expr
case class Prod(val objs: SComplex[Expr], val exprs: STree[Expr]) extends Expr
case class Hom(val objs: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr

// Dim 2
case class Comp(val base: SComplex[Expr], val exprs: STree[Expr]) extends Expr
case class Pair(val objs: SComplex[Expr], val exprs: STree[Expr]) extends Expr
case class App(val objs: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr
case class Lam(val base: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr

// Dim >= 3
case class CompFill(val base: SComplex[Expr], val exprs: STree[Expr]) extends Expr
case class LamFill(val base: SComplex[Expr], val deriv: SDeriv[Expr], val tgt: Expr) extends Expr
case class Univ(val cmplx: SComplex[Option[Expr]]) extends Expr

object Expr {

  def toComplex(e: Expr): SComplex[Expr] =
    e match {
      case Obj => ||(SDot(Obj))
      case Var(id) => ||(SBox(Obj, SNode(SDot(Obj), SLeaf))) >> SDot(e)
      case Prod(objs, exprs) => toComplex(Pair(objs, exprs)).target.get
      case Pair(objs, exprs) => objs >> SBox(Prod(objs, exprs), exprs.map(SDot(_))) >> SDot(e)
      case App(objs, deriv, tgt) => objs >> SBox(tgt, deriv.plug(Hom(objs, deriv,tgt)).map(SDot(_))) >> SDot(e)
      case Hom(objs, deriv, tgt) => toComplex(App(objs, deriv, tgt)).target.get
      case Comp(base, pd) => toComplex(CompFill(base, pd)).target.get
      case CompFill(base, pd) => base >> SBox(Comp(base, pd), pd.map(SDot(_))) >> SDot(e)
      case _ => ||(SDot(Obj)) // Dummy value ...
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
        case p:Prod => CellRendering(text(p.toString), colorSpec = ProdColorSpec)
        case p:Pair => CellRendering(text(p.toString), colorSpec = PairColorSpec)
        case h:Hom => CellRendering(text(h.toString), colorSpec = HomColorSpec)
        case a:App => CellRendering(text(a.toString), colorSpec = AppColorSpec)
        case c:Comp => CellRendering(text(c.toString), colorSpec = ProdColorSpec)
        case c:CompFill => CellRendering(text(c.toString), colorSpec = PairColorSpec)
        case _ => CellRendering(spacer(Bounds(0, 0, 600, 600)))
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

  object ProdColorSpec extends ColorSpec(
    fill = "#FFFFFF",
    fillHovered = "#ffa8a8", // "#F3F4F5",
    fillSelected = "#ff7d7d",
    stroke = "#d6713e",
    strokeHovered = "#d6713e",
    strokeSelected = "#d6713e",
    edgeHovered = "#f19091"
  )

  object PairColorSpec extends ColorSpec(
    fill = "#FFFFFF",
    fillHovered = "#ffa8a8", // "#F3F4F5",
    fillSelected = "#ff7d7d",
    stroke = "#3b8716",
    strokeHovered = "#3b8716",
    strokeSelected = "#3b8716",
    edgeHovered = "#f19091"
  )

  object HomColorSpec extends ColorSpec(
    fill = "#FFFFFF",
    fillHovered = "#ffa8a8", // "#F3F4F5",
    fillSelected = "#ff7d7d",
    stroke = "#303e80",
    strokeHovered = "#303e80",
    strokeSelected = "#303e80",
    edgeHovered = "#f19091"
  )

  object AppColorSpec extends ColorSpec(
    fill = "#FFFFFF",
    fillHovered = "#ffa8a8", // "#F3F4F5",
    fillSelected = "#ff7d7d",
    stroke = "#698030",
    strokeHovered = "#698030",
    strokeSelected = "#698030",
    edgeHovered = "#f19091"
  )


}
