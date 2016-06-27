/**
  * Syntax.scala - Syntax for Scala OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopictt

// Expressions
sealed trait Expr

case object EType extends Expr
case object EUnit extends Expr
case object ETt extends Expr
case object EId extends Expr

case object EMnd extends Expr
case object EIndexOf extends Expr
case object EConsOf extends Expr

case object EEta extends Expr
case object EEtaI extends Expr
case object EEtaE extends Expr

case object EMu extends Expr
case object EMuI extends Expr
case object EMuE extends Expr

case object ESl extends Expr
case object ESlCn extends Expr
case object EDot extends Expr
case object EBox extends Expr

case class EDec(d: Decl, e: Expr) extends Expr
case class EVar(id: Ident) extends Expr
case class EPi(p: Patt, e: Expr, t: Expr) extends Expr
case class ELam(p: Patt, e: Expr) extends Expr
case class EApp(e: Expr, f: Expr) extends Expr
case class ESig(p: Patt, e: Expr, t: Expr) extends Expr
case class EPair(e: Expr, f: Expr) extends Expr
case class EFst(e: Expr) extends Expr
case class ESnd(e: Expr) extends Expr

case class EPhi(p: Patt, m: Expr, i: Expr, c: Expr, e: Expr) extends Expr
case class EKap(p: Patt, e: Expr) extends Expr
case class ESel(d: Expr, p: Expr) extends Expr
case class ETypeOf(p: Expr) extends Expr

// Values
sealed trait Val

case object Type extends Val
case object Unt extends Val
case object Id extends Val
case object Tt extends Val

case class Pi(v: Val, c: Clos) extends Val
case class Lam(c: Clos) extends Val
case class Kap(c: Clos) extends Val
case class Sig(v: Val, c: Clos) extends Val
case class Pair(v: Val, w: Val) extends Val
case class Nt(n: Neut) extends Val

// Place quantification
case class Phi(m: Val, i: Val, c: Val, g: Clos) extends Val
case class PlaceOf(m: Val, i: Val, c: Val) extends Val
case class EtaP(m: Val, i: Val) extends Val
case class MuP(m: Val, p: Val, q: Val) extends Val  
case class TypeOf(p: Val) extends Val

// Neutral terms
sealed trait Neut
case class Gen(i: Int, n: Name) extends Neut
case class App(n: Neut, nf: Nf) extends Neut
case class Sel(n: Neut, nf: Nf) extends Neut
case class Fst(n: Neut) extends Neut
case class Snd(n: Neut) extends Neut

case object Mnd extends Neut
case object IndexOf extends Neut
case object ConsOf extends Neut

case object Eta extends Neut
case object EtaI extends Neut
case object EtaE extends Neut

case object Mu extends Neut
case object MuI extends Neut
case object MuE extends Neut

case object Sl extends Neut
case object SlCn extends Neut
case object Dot extends Neut
case object Box extends Neut

// Patterns
sealed trait Patt
case object Punit extends Patt
case class PVar(id: Ident) extends Patt
case class PPair(p: Patt, q: Patt) extends Patt

// Declarations
sealed trait Decl
case class Def(p: Patt, e: Expr, f: Expr) extends Decl
case class Drec(p: Patt, e: Expr, f: Expr) extends Decl
case class Dnorm(e: Expr) extends Decl

// Function closures
sealed trait Clos
case class Cl(p: Patt, e: Expr, rho: Rho) extends Clos

// Environment
sealed trait Rho
case object RNil extends Rho
case class UpVar(rho: Rho, p: Patt, v: Val) extends Rho
case class UpDec(rho: Rho, d: Decl) extends Rho
