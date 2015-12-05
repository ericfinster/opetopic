/**
  * Property.scala - A class for encapsulating a property
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

import opetopic._
import opetopic.tt._

sealed trait Property[N <: Nat] {

  def id: String
  def cell: Cell[N]
  def expr: Expr
  def ty: Expr
  def dim: N = cell.dim

}

case class IsLeftExtension[P <: Nat](
  val id: String,
  val expr: Expr,
  val cell: Cell[S[P]]
) extends Property[S[P]] {
  val ty = ELeftExt(cell.expr)
}

case class IsRightExtension[P <: Nat](
  val id: String,
  val expr: Expr,
  val cell: Cell[S[P]],
  val addr: Address[P]
) extends Property[S[P]] {
  val ty = ERightExt(cell.expr, OpetopicTypeChecker.rbAddr(dim.pred)(addr))
}
