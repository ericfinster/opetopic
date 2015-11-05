/**
  * Parser.scala - Parser for OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import opetopic._
import TypeLemmas._

import scala.util.parsing.combinator._
import scala.util.parsing.input._

object OpetopicParser extends RegexParsers with PackratParsers {

  lazy val ident: Parser[String] = 
    """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r

  lazy val expr: PackratParser[Expr] = (
      expr1 ~ "," ~ expr ^^
        { case e ~ "," ~ f => EPair(e, f) }
    | expr1
  )

  lazy val expr1: PackratParser[Expr] = (
      "\\" ~ pattern1 ~ "." ~ expr1 ^^
        { case "\\" ~ p ~ "." ~ e => ELam(p, e) }
    | "(" ~ pattern1 ~ ":" ~ expr1 ~ ") -> " ~ expr1 ^^
        { case "(" ~ p ~ ":" ~ e ~ ") -> " ~ f => EPi(p, e, f) }
    | "(" ~ pattern1 ~ ":" ~ expr1 ~ ") * " ~ expr1 ^^
        { case "(" ~ p ~ ":" ~ e ~ ") * " ~ f => ESig(p, e, f) }
    | decl ~ ";" ~ expr1 ^^ 
        { case d ~ ";" ~ e => EDec(d, e) }
    | expr2 ~ "->" ~ expr1 ^^ 
        { case e ~ "->" ~ f => EPi(Punit, e, f) }
    | expr2 ~ "*" ~ expr1 ^^
        { case e ~ "*" ~ f => ESig(Punit, e, f) }
    | cmplxExpr(Z) ^^ 
        { case nl => ECmplx(nstListToComplex(intToNat(nl.length - 1))(nl.reverse)) }
    | expr2
  )

  lazy val expr2: PackratParser[Expr] = (
      expr2 ~ expr3 ^^
        { case e ~ f => EApp(e, f) }
    | expr3
  )

  lazy val expr3: PackratParser[Expr] = (
      "Unit" ^^^ EUnit
    | "Type" ^^^ EType
    | "tt" ^^^ ETt
    | ident ^^ (EVar(_))
    | expr3 ~ ".1" ^^ 
        { case e ~ ".1" => EFst(e) }
    | expr3 ~ ".2" ^^ 
        { case e ~ ".2" => ESnd(e) }
    | "(" ~ expr ~ ")" ^^
        { case "(" ~ e ~ ")" => e }
  )

  @natElim
  def nstListToComplex[N <: Nat](n: N)(nl: NstList) : Complex[CstExpr, N] = {
    case (Z, n :: Nil) => {
      Complex[CstExpr] >> n.value.asInstanceOf[ExprNst[_0]]
    }
    case (Z, _) => ???
    case (S(p: P), n :: ns) => {
      nstListToComplex(p)(ns) >> n.value.asInstanceOf[ExprNst[S[P]]]
    }
    case (S(p: P), _) => ???
  }

  def treeExpr[A, N <: Nat](n: N)(pp: PackratParser[A]) : PackratParser[Tree[A, N]] = (
      treeExpr1(n)(pp)
    | "(" ~ treeExpr(n)(pp) ~ ")" ^^
        { case "(" ~ t ~ ")" => t }
  )

  @natElim
  def treeExpr1[A, N <: Nat](n: N)(pp: PackratParser[A]) : PackratParser[Tree[A, N]] = {
    case (Z, pp) => "pt" ~ pp ^^ { case "pt" ~ a => Pt(a) }
    case (S(p), pp) => (
        "leaf" ^^^ Leaf(S(p))
      | "node" ~ pp ~ treeExpr(p)(treeExpr(S(p))(pp)) ^^
        { case "node" ~ a ~ sh => Node(a, sh) }
    )
  }

  def nstExpr[A, N <: Nat](n: N)(pp: PackratParser[A]) : PackratParser[Nesting[A, N]] = (
      nstExpr1(n)(pp)
    | "(" ~ nstExpr(n)(pp) ~ ")" ^^
        { case "(" ~ n ~ ")" => n }
  )

  @natElim
  def nstExpr1[A, N <:  Nat](n: N)(pp: PackratParser[A]) : PackratParser[Nesting[A, N]] = {
    case (Z, pp) => (
        "obj" ~ pp ^^ { case "obj" ~ a => Obj(a) }
      | "box" ~ pp ~ treeExpr(Z)(nstExpr(Z)(pp)) ^^ { case "box" ~ a ~ sh => Box(a, sh) }
    )
    case (S(p), pp) => (
        "dot" ~ pp ^^ { case "dot" ~ a => Dot(a, S(p)) }
      | "box" ~ pp ~ treeExpr(S(p))(nstExpr(S(p))(pp)) ^^ { case "box" ~ a ~ sh => Box(a, sh) }
    )
  }

  def cmplxExpr[N <: Nat](n: N) : PackratParser[NstList] = (
      "[" ~ nstExpr(n)(expr3) ~ "]>>" ~ cmplxExpr(S(n)) ^^
        { case "[" ~ nst ~ "]>>" ~ tl => Sigma[ExprNst, N](n)(nst) :: tl }
    | nstExpr(n)(expr3) ^^ 
        { case nst => List(Sigma[ExprNst, N](n)(nst)) }
  )

  lazy val decl: PackratParser[Decl] = (
      "let" ~ pattern ~ ":" ~ expr ~ "=" ~ expr ^^ 
        { case "let" ~ p ~ ":" ~ e ~ "=" ~ f => Def(p, e, f) }
    | "letrec" ~ pattern ~ ":" ~ expr ~ "=" ~ expr ^^ 
        { case "letrec" ~ p ~ ":" ~ e ~ "=" ~ f => Drec(p, e, f) }
  )

  lazy val pattern: PackratParser[Patt] = (
      pattern1 ~ "," ~ pattern ^^
        { case p ~ "," ~ q => PPair(p, q) }
    | pattern1
  )

  lazy val pattern1: PackratParser[Patt] = (
      "_" ^^^ Punit
    | ident ^^ { PVar(_) }
    | "(" ~ pattern ~ ")" ^^ 
        { case "(" ~ p ~ ")" => p }
  )

  override protected val whiteSpace = """(\s|--.*)+""".r

}
