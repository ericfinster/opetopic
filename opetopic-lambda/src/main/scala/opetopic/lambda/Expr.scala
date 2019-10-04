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
case object Obj

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

// A marker class for displaying expressions...
case class ExprMarker(
  val expr: Expr
) extends SimpleMarker(expr.toString)
