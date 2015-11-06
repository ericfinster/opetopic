/**
  * Syntax.scala - Syntax of OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import opetopic._

// Expressions
sealed trait Expr
case object EType extends Expr
case object EUnit extends Expr
case object ETt extends Expr

// Type theoretic constructors
case class EVar(id: Ident) extends Expr
case class ELam(p: Patt, e: Expr) extends Expr
case class EPi(p: Patt, e: Expr, t: Expr) extends Expr
case class ESig(p: Patt, e: Expr, t: Expr) extends Expr
case class EPair(e: Expr, f: Expr) extends Expr
case class EFst(e: Expr) extends Expr
case class ESnd(e: Expr) extends Expr
case class EDec(d: Decl, e: Expr) extends Expr
case class EApp(e: Expr, f: Expr) extends Expr

case class EFrm[N <: Nat](c: Complex[CstExpr, N]) extends Expr

case class EComp[N <: Nat](fp: Suite[NstExpr, N], nch: TrExpr[N]) extends Expr
case class EFill[N <: Nat](fp: Suite[NstExpr, N], nch: TrExpr[N]) extends Expr

// Values
sealed trait Val
case object Type extends Val
case object Unt extends Val
case object Tt extends Val
case class Lam(c: Clos) extends Val
case class Pair(v: Val, w: Val) extends Val
case class Pi(v: Val, c: Clos) extends Val
case class Sig(v: Val, c: Clos) extends Val
case class Nt(n: Neut) extends Val
case class Frm[N <: Nat](c: Complex[CstVal, N]) extends Val
case class Comp[N <: Nat](fp: Suite[NstVal, N], nch: TrVal[N]) extends Val
case class Fill[N <: Nat](fp: Suite[NstVal, N], nch: TrVal[N]) extends Val

// Neutral terms
sealed trait Neut
case class Gen(i: Int, n: Name) extends Neut
case class App(n: Neut, nf: Nf) extends Neut
case class Fst(n: Neut) extends Neut
case class Snd(n: Neut) extends Neut

// Patterns
sealed trait Patt
case object Punit extends Patt
case class PVar(id: Ident) extends Patt
case class PPair(p: Patt, q: Patt) extends Patt

// Declarations
sealed trait Decl
case class Def(p: Patt, e: Expr, f: Expr) extends Decl
case class Drec(p: Patt, e: Expr, f: Expr) extends Decl

// Function closures
sealed trait Clos
case class Cl(p: Patt, e: Expr, rho: Rho) extends Clos

// Environment
sealed trait Rho
case object RNil extends Rho
case class UpVar(rho: Rho, p: Patt, v: Val) extends Rho
case class UpDec(rho: Rho, d: Decl) extends Rho
