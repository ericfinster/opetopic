/**
  * Syntax.scala - Syntax for Scala MiniTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.newtt

import opetopic._

// Expressions
sealed trait Expr
case object EType extends Expr
case object EUnit extends Expr
case object ETt extends Expr
case object EEmpty extends Expr

case class ELam(p: Patt, e: Expr) extends Expr
case class EPi(p: Patt, e: Expr, t: Expr) extends Expr
case class ESig(p: Patt, e: Expr, t: Expr) extends Expr
case class EPair(e: Expr, f: Expr) extends Expr
case class EFst(e: Expr) extends Expr
case class ESnd(e: Expr) extends Expr
case class EApp(e: Expr, f: Expr) extends Expr
case class EVar(id: Ident) extends Expr
case class EDec(d: Decl, e: Expr) extends Expr

// Tree Expressions
case class EPt(e: Expr) extends Expr
case object ELf extends Expr
case class ENd(e: Expr, sh: Expr) extends Expr

// Categories and Cells
case object ECat extends Expr
case class EOb(c: Expr) extends Expr
case class ECell(c: Expr, d: Nat, s: Expr, t: Expr) extends Expr
case class EHom(c: Expr, d: Nat, s: Expr, t: Expr) extends Expr

// Patterns
sealed trait Patt
case object Punit extends Patt
case class PVar(id: Ident) extends Patt
case class PPair(p: Patt, q: Patt) extends Patt

// Declarations
sealed trait Decl
case class Def(p: Patt, e: Expr, f: Expr) extends Decl
case class Drec(p: Patt, e: Expr, f: Expr) extends Decl

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
case class Nt(n: Neut) extends Val

// Tree Values
case object VLf extends Val
case class VPt(v: Val) extends Val
case class VNd(v: Val, sh: Val) extends Val

// Category and Cell Values
case object Cat extends Val
case class Ob(cv: Val) extends Val
case class Cell(c: Val, d: Nat, s: Val, t: Val) extends Val
case class Hom(c: Val, d: Nat, s: Val, t: Val) extends Val

// Neutral terms
sealed trait Neut
case class Gen(i: Int, n: Name) extends Neut
case class App(n: Neut, nf: Nf) extends Neut
case class Fst(n: Neut) extends Neut
case class Snd(n: Neut) extends Neut

// Function closures
sealed trait Clos
case class Cl(p: Patt, e: Expr, rho: Rho) extends Clos

// Environment
sealed trait Rho
case object RNil extends Rho
case class UpVar(rho: Rho, p: Patt, v: Val) extends Rho
case class UpDec(rho: Rho, d: Decl) extends Rho

