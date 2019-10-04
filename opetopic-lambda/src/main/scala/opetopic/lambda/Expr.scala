/**
  * Expr.scala - Expressions for planar lambda calculus
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.lambda

import opetopic._
import opetopic.ui._

sealed trait Expr

// Dim 0 
case object Obj extends Expr

// Dim 1
case class Var(val id: String) extends Expr
case class Prod(val cmplx: SComplex[Option[Expr]]) extends Expr
case class Hom(val cmplx: SComplex[Option[Expr]]) extends Expr

// Dim 2
case class Comp(val cmplx: SComplex[Option[Expr]]) extends Expr
case class Pair(val cmplx: SComplex[Option[Expr]]) extends Expr
case class App(val cmplx: SComplex[Option[Expr]]) extends Expr
case class Lam(val cmplx: SComplex[Option[Expr]]) extends Expr

// Dim >= 3
case class Univ(val cmplx: SComplex[Option[Expr]]) extends Expr

object Expr {

  def toComplex(e: Expr): SComplex[Expr] =
    e match {
      case Obj => ||(SDot(Obj))
      case Var(id) => ||(SBox(Obj, SNode(SDot(Obj), SLeaf))) >> SDot(e)
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
        case Var(id) => CellRendering(text(id), colorSpec = VariableColorSpec)
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
    fillHovered = "#DB2828", // "#F3F4F5",
    fillSelected = "#DCDDDE",
    stroke = "#ba5050",
    strokeHovered = "#ba5050",
    strokeSelected = "#ba5050",
    edgeHovered = "#f19091"
  )

}
