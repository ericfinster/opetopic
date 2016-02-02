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
  var isLeftExt : Option[Property[N]] = None
  var isRightExt : Option[Property[N]] = None
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

  object ActiveInstance {

    import opetopic.js.JsDomFramework._

    // implicit def cellAffixable[N <: Nat] : Affixable[Cell[N]] = 
    //   new Affixable[Cell[N]] {
    //     type ElementType = TextType
    //     def decoration(cell: Cell[N]) =
    //       (cell.isLeftExt, cell.isRightExt) match {
    //         case (None, None) =>
    //           cell.expr match {
    //             case EVar(_) => Decoration(text(cell.id)) //, "variable")
    //             case _ => Decoration(text(cell.id)) //, "composite")
    //           }
    //         case (Some(_), None) => Decoration(text(cell.id)) //, "left-extension")
    //         case (None, Some(_)) => Decoration(text(cell.id)) // , "right-extension")
    //         case (Some(_), Some(_)) => Decoration(text(cell.id)) //, "dual-extension")
    //       }
    //   }

    // implicit object CellAffixableFamily extends AffixableFamily[Cell] {
    //   def apply[N <: Nat](n: N) : Affixable[Cell[N]] = cellAffixable[N]
    // }

    implicit def cellVisualizable[N <: Nat] : Visualizable[Cell[N], N] = ???
    implicit val cellVisualizableFamilt : VisualizableFamily[Cell] = ???

  }

  object StaticInstance {

    import opetopic.ui.ScalatagsTextFramework._

    implicit val cellVisualizable : VisualizableFamily[Cell] = ???

    // implicit object StaticCellAffixableFamily extends AffixableFamily[Cell] {
    //   def apply[N <: Nat](n: N) : Affixable[Cell[N]] =
    //     new Affixable[Cell[N]] {
    //       type ElementType = TextType
    //       def decoration(cell: Cell[N]) =
    //         (cell.isLeftExt, cell.isRightExt) match {
    //           case (None, None) => 
    //             cell.expr match {
    //               case EVar(_) => Decoration(text(cell.id)) //, "variable")
    //               case _ => Decoration(text(cell.id)) //, "composite")
    //             }
    //           case (Some(_), None) => Decoration(text(cell.id)) //, "left-extension")
    //           case (None, Some(_)) => Decoration(text(cell.id)) //, "right-extension")
    //           case (Some(_), Some(_)) => Decoration(text(cell.id)) //, "dual-extension")
    //         }
    //     }
    // }

  }

}
