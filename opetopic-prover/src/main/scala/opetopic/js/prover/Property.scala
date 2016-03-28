/**
  * Property.scala - Encapsulate properties of cell expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import opetopic.tt._

sealed trait Property {
  def propertyId: String
  def propertyExpr: Expr
  def propertyType: Expr
  def cellId: String
  def cellExpr: Expr
  def isLeft: Boolean
  def isRightAt(a: Addr): Boolean
}

case class LeftExtensionProperty(
  val propertyId: String,
  val propertyExpr: Expr,
  val propertyType: Expr,
  val cellId: String,
  val cellExpr: Expr
) extends Property {
  def isLeft: Boolean = true
  def isRightAt(a: Addr): Boolean = false
}

case class RightExtensionProperty(
  val propertyId: String,
  val propertyExpr: Expr,
  val propertyType: Expr,
  val cellId: String,
  val cellExpr: Expr
) extends Property {
  def isLeft: Boolean = false
  def isRightAt(a: Addr): Boolean = false
}
