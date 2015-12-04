/**
  * Cell.scala - Representation of cell data
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

import opetopic._
import opetopic.tt._
import syntax.complex._

sealed trait Cell[N <: Nat] {
  def id: String
  def face: Complex[Cell, N]
  def expr: Expr
  def ty: Expr
  def dim: N
  var isLeftExt: Option[Expr] = None
  var isRightExt: Option[Expr] = None
}

case class ObjectCell(
  val id: String,
  val expr: Expr
) extends Cell[_0] {
  val face = Complex[Cell] >> Obj(this)
  val ty = EOb(EVar("X"))
  val dim = Z
}

case class HigherCell[P <: Nat](
  val id: String,
  val expr: Expr,
  val frm: Complex[Cell, P]
) extends Cell[S[P]] {
  val dim = frm.length
  val face = frm >> Dot(this, dim)
  val frmExpr : ExprComplex[P] = frm.map(Cell.CellToExpr)
  val ty = ECell(EVar("X"), frmExpr)
}

object Cell {

  type CNst[N <: Nat] = Nesting[Cell[N], N]

  type OptCell[N <: Nat] = Option[Cell[N]]
  type OptCellCmplx[N <: Nat] = Complex[OptCell, N]

  @natElim
  def apply[N <: Nat](n: N)(id: String, expr: Expr, suite: Suite[CNst, N]) : Cell[N] = {
    case (Z, id, expr, _) => ObjectCell(id, expr)
    case (S(p: P), id, expr, suite) => HigherCell[P](id, expr, suite)
  }

  object CellToExpr extends IndexedMap[Cell, ConstExpr] {
    def apply[N <: Nat](n: N)(cell: Cell[N]) : Expr = 
      cell.expr
  }

}
