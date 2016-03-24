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
case object EEmpty extends Expr

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
case class ERec(fs: List[Field]) extends Expr
case class EProj(id: Ident, e: Expr) extends Expr

case class Field(id: Ident, e: Expr)

// Categories and Cells
case object ECat extends Expr
case class EOb(e: Expr) extends Expr
case class ECell[N <: Nat](e: Expr, c: ExprComplex[N]) extends Expr
case class EHom[N <: Nat](e: Expr, c: ExprComplex[N]) extends Expr

// Fillers and composition
case class EComp[N <: Nat](e: Expr, fp: Suite[NstExpr, N], nch: TrExpr[N]) extends Expr
case class EFill[N <: Nat](e: Expr, fp: Suite[NstExpr, N], nch: TrExpr[N]) extends Expr

// Cell properties
case class EIsLeftExt(e: Expr) extends Expr
case class EIsRightExt(e: Expr, a: Addr) extends Expr 

// Property constructors
case class EFillIsLeft[N <: Nat](e: Expr, fp: Suite[NstExpr, N], nch: TrExpr[N]) extends Expr
case class EShellIsLeft[N <: Nat](ce: Expr, frm: ExprComplex[N], e:Expr, ev: Expr) extends Expr

// Left and right lifting expressions
// case class ELiftLeft[N <: Nat](ce: Expr, frm: ExprComplex[N], e: Expr, ev: Expr) extends Expr
case class ELiftLeft(e: Expr, ev: Expr) extends Expr
case class EFillLeft[N <: Nat](ce: Expr, frm: ExprComplex[N], e: Expr, ev: Expr) extends Expr
case class EFillLeftIsLeft[N <: Nat](ce: Expr, frm: ExprComplex[N], e: Expr, ev: Expr) extends Expr
case class EFillLeftIsRight[N <: Nat](ce: Expr, frm: ExprComplex[N], e: Expr, ev: Expr) extends Expr
case class ELiftRight[N <: Nat](ce: Expr, frm: ExprComplex[N], e: Expr, ev: Expr, a:Addr) extends Expr
case class EFillRight[N <: Nat](ce: Expr, frm: ExprComplex[N], e: Expr, ev: Expr, a:Addr) extends Expr
case class EFillRightIsLeft[N <: Nat](ce: Expr, frm: ExprComplex[N], e: Expr, ev: Expr, a:Addr) extends Expr
case class EFillRightIsRight[N <: Nat](ce: Expr, frm: ExprComplex[N], e: Expr, ev: Expr, a:Addr) extends Expr

// Values
sealed trait Val
case object Type extends Val
case object Unt extends Val
case object Tt extends Val
case object Empty extends Val
case class Lam(c: Clos) extends Val
case class Pair(v: Val, w: Val) extends Val
case class Pi(v: Val, c: Clos) extends Val
case class Sig(v: Val, c: Clos) extends Val
case class Proj(f: Name, v: Val) extends Val
case class Rec(sc: SClos) extends Val
case class Nt(n: Neut) extends Val

case object Cat extends Val
case class Ob(cv: Val) extends Val
case class Cell[N <: Nat](cv: Val, fv: ValComplex[N]) extends Val
case class Hom[N <: Nat](cv: Val, fv: ValComplex[N]) extends Val

case class Comp[N <: Nat](v: Val, fp: Suite[NstVal, N], nch: TrVal[N]) extends Val
case class Fill[N <: Nat](v: Val, fp: Suite[NstVal, N], nch: TrVal[N]) extends Val

case class IsLeftExt(v: Val) extends Val
case class IsRightExt(v: Val, a: Addr) extends Val

case class FillIsLeft[N <: Nat](v: Val, fp: Suite[NstVal, N], nch: TrVal[N]) extends Val
case class ShellIsLeft[N <: Nat](v: Val) extends Val

// Neutral terms
sealed trait Neut
case class Gen(i: Int, n: Name) extends Neut
case class App(n: Neut, nf: Nf) extends Neut
case class Fst(n: Neut) extends Neut
case class Snd(n: Neut) extends Neut

// Property deconstructors are neutral ...
case class LiftLeft(v: Val, ev: Val) extends Neut
case class FillLeft(v: Val, ev: Val) extends Neut
case class FillLeftIsLeft(v: Val, ev: Val) extends Neut
case class FillLeftIsRight(v: Val, ev: Val) extends Neut
case class LiftRight(v: Val, ev: Val, a: Addr) extends Neut
case class FillRight(v: Val, ev: Val, a: Addr) extends Neut
case class FillRightIsLeft(v: Val, ev: Val, a: Addr) extends Neut
case class FillRightIsRight(v: Val, ev: Val, a: Addr) extends Neut

// Addresses
sealed trait Addr
case object AUnit extends Addr
case object ANil extends Addr
case class ACons(a: Addr, b: Addr) extends Addr

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
