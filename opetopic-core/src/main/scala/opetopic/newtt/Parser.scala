/**
  * Parser.scala - Scala MiniTT Parser
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.newtt

import scala.util.parsing.combinator._
import scala.util.parsing.input._

import opetopic._

object OTTParser extends RegexParsers with PackratParsers {

  lazy val ident: Parser[String] = 
    """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r

  lazy val intLit: Parser[String] = 
    """([0-9])+""".r

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
    | "isLeftExt" ~ expr3 ^^
        { case "isLeftExt" ~ e => EIsLeftExt(e) }
    | "isRightExt" ~ expr3 ~ addrExpr ^^
        { case "isRightExt" ~ e ~ ae => EIsRightExt(e, ae) }
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
    | "empty" ^^^ EEmpty
    | "tt" ^^^ ETt
    | "Cat" ^^^ ECat
    | "Ob" ~ expr3 ^^ 
        { case "Ob" ~ c => EOb(c) }
    | "Cell" ~ expr3 ~ intLit ~ trExpr ~ expr3 ^^ 
        { case "Cell" ~ c ~ d ~ s ~ t => ECell(c, intToNat(d.toInt - 1), s, t) }
    | "Hom" ~ expr3 ~ intLit ~ trExpr ~ expr3 ^^
        { case "Hom" ~ c ~ d ~ s ~ t => EHom(c, intToNat(d.toInt - 1), s, t) }
    | trExpr
    | expr3 ~ ".1" ^^ 
        { case e ~ ".1" => EFst(e) }
    | expr3 ~ ".2" ^^ 
        { case e ~ ".2" => ESnd(e) }
    | ident ^^ { EVar(_) }
    | "(" ~ expr ~ ")" ^^
        { case "(" ~ e ~ ")" => e }
  )

  lazy val trExpr: PackratParser[Expr] = (
      "pt" ~ expr3 ^^
        { case "pt" ~ e => EPt(e) }
    | "lf" ^^^ ELf
    | "nd" ~ expr3 ~ trExpr ^^
        { case "nd" ~ e ~ sh => ENd(e, sh) }
    | "(" ~ trExpr ~ ")" ^^
        { case "(" ~ te ~ ")" => te }
  )

  lazy val addrExpr : PackratParser[Addr] = (
      addrExpr1 ~ "::" ~ addrExpr ^^
        { case a ~ "::" ~ b => ACons(a, b) }
    | addrExpr1
  )

  lazy val addrExpr1 : PackratParser[Addr] = (
      "#" ^^^ AUnit
    | "nil" ^^^ ANil
    | "(" ~ addrExpr ~ ")" ^^ 
        { case "(" ~ a ~ ")" => a }
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

  lazy val decl: PackratParser[Decl] = (
      "let" ~ pattern ~ ":" ~ expr ~ "=" ~ expr ^^ 
        { case "let" ~ p ~ ":" ~ e ~ "=" ~ f => Def(p, e, f) }
    | "letrec" ~ pattern ~ ":" ~ expr ~ "=" ~ expr ^^ 
        { case "letrec" ~ p ~ ":" ~ e ~ "=" ~ f => Drec(p, e, f) }
  )

  override protected val whiteSpace = """(\s|--.*)+""".r

}
