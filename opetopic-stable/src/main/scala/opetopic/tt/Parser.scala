/**
  * Parser.scala - Scala MiniTT Parser
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import fastparse.WhitespaceApi

import opetopic._

object Parser {

  val Whitespace = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" \r\t\n").rep)
  }

  import fastparse.noApi._
  import Whitespace._

  val letter     = P( lowercase | uppercase )
  val lowercase  = P( CharIn('a' to 'z') )
  val uppercase  = P( CharIn('A' to 'Z') )
  val digit      = P( CharIn('0' to '9') )

  // This is bad!
  val ident: Parser[String] = 
    P( CharsWhile(_.isLetterOrDigit) ).!

  //============================================================================================
  // EXPRESSIONS
  //


  // Expr

  val expr: Parser[Expr] = 
    P( pair | expr1 )

  val pair: Parser[Expr] = 
    P( expr1 ~ "," ~ expr ).map({
      case (e, f) => EPair(e, f)
    })

  // Expr1

  val expr1: Parser[Expr] = 
    P( lambda | pi | sigma | arrow | product | expr2 )

  val lambda: Parser[Expr] = 
    P( "\\" ~ pattern1 ~ "." ~ expr ).map({ 
      case (p, e) => ELam(p, e) 
    })

  val pi: Parser[Expr] = 
    P( "(" ~ pattern1 ~ ":" ~ expr ~ ")" ~ "->" ~ expr ).map({ 
      case (p, e, f) => EPi(p, e, f) 
    })

  val sigma: Parser[Expr] = 
    P( "(" ~ pattern1 ~ ":" ~ expr ~ ")" ~ "*" ~ expr ).map({ 
      case (p, e, f) => ESig(p, e, f) 
    })

  val arrow: Parser[Expr] = 
    P( expr2 ~ "->" ~ expr ).map({
      case (e, f) => EPi(Punit, e, f)
    })

  val product: Parser[Expr] = 
    P( expr2 ~ "*" ~ expr ).map({
      case (e, f) => EPi(Punit, e, f)
    })

  // Expr2

  val expr2: Parser[Expr] =
    P( expr3 ~ expr.rep(1).? ).map({
      case (e, None) => e
      case (e, Some(s)) => s.foldLeft(e)(EApp(_, _))
    })

  // Expr3

  val expr3: Parser[Expr] = 
    P( ident.map(EVar(_)) | "(" ~ expr ~ ")" )

  //============================================================================================
  // PATTERNS
  //

  val pattern: Parser[Patt] = 
    P( pattern1 ~ ( "," ~ pattern ).? ).map({ 
      case (p, None) => p
      case (p, Some(q)) => PPair(p, q)
    })

  val pattern1: Parser[Patt] = 
    P(  "_".!.map(_ => Punit) 
      | ident.!.map(PVar(_))
      | "(" ~ pattern ~ ")"
    )

  //============================================================================================
  // DECLARATIONS
  //

  val decl: Parser[Decl] = 
    P( simpleDecl | recursiveDecl )

  val simpleDecl: Parser[Decl] = 
    P( "let" ~ pattern ~ ":" ~ expr ~ "-" ~ expr ).map({
      case (p, e, f) => Def(p, e, f)
    })

  val recursiveDecl: Parser[Decl] = 
    P( "letrec" ~ pattern ~ ":" ~ expr ~ "-" ~ expr ).map({
      case (p, e, f) => Drec(p, e, f)
    })

}


// import scala.util.parsing.combinator._
// import scala.util.parsing.input._

// import opetopic._

// object OTTParser extends RegexParsers with PackratParsers {

//   lazy val ident: Parser[String] = 
//     """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r

//   lazy val intLit: Parser[String] = 
//     """([0-9])+""".r

//   lazy val expr: PackratParser[Expr] = (
//       expr1 ~ "," ~ expr ^^
//         { case e ~ "," ~ f => EPair(e, f) }
//     | expr1
//   )

//   lazy val expr1: PackratParser[Expr] = (
//       "\\" ~ pattern1 ~ "." ~ expr1 ^^
//         { case "\\" ~ p ~ "." ~ e => ELam(p, e) }
//     | "(" ~ pattern1 ~ ":" ~ expr1 ~ ") -> " ~ expr1 ^^
//         { case "(" ~ p ~ ":" ~ e ~ ") -> " ~ f => EPi(p, e, f) }
//     | "(" ~ pattern1 ~ ":" ~ expr1 ~ ") * " ~ expr1 ^^
//         { case "(" ~ p ~ ":" ~ e ~ ") * " ~ f => ESig(p, e, f) }
//     | decl ~ ";" ~ expr1 ^^ 
//         { case d ~ ";" ~ e => EDec(d, e) }
//     | expr2 ~ "->" ~ expr1 ^^ 
//         { case e ~ "->" ~ f => EPi(Punit, e, f) }
//     | expr2 ~ "*" ~ expr1 ^^
//         { case e ~ "*" ~ f => ESig(Punit, e, f) }
//     | expr2
//   )

//   lazy val expr2 : PackratParser[Expr] = (
//       "Ob" ~ expr4 ^^ 
//         { case "Ob" ~ c => EOb(c) }
//     | "Cell" ~ expr4 ~ complexExpr ^^ 
//         { case "Cell" ~ c ~ e => ECell(c, e) }
//     | "isLeftExt" ~ expr4 ^^
//         { case "isLeftExt" ~ e => EIsLeftExt(e) }
//     | "isRightExt" ~ expr4 ~ addrExpr ^^
//         { case "isRightExt" ~ e ~ ae => EIsRightExt(e, ae) }
//     | "refl" ~ expr4 ~ expr4 ^^
//         { case "refl" ~ c ~ e => ERefl(c, e) }
//     | "drop" ~ expr4 ~ expr4 ^^
//         { case "drop" ~ c ~ e => EDrop(c, e) }
//     | "comp" ~ expr4 ~ intLit ~ expr4 ^^
//         { case "comp" ~ c ~ d ~ pd => EComp(c, d.toInt, pd) }
//     | "fill" ~ expr4 ~ intLit ~ expr4 ^^
//         { case "fill" ~ c ~ d ~ pd => EFill(c, d.toInt, pd) }
//     | "liftLeft" ~ expr3 ~ expr3 ~ expr3 ~ expr3 ^^
//         { case "liftLeft" ~ e ~ ev ~ c ~ t => ELiftLeft(e, ev, c, t) }
//     | "fillLeft" ~ expr3 ~ expr3 ~ expr3 ~ expr3 ^^
//         { case "fillLeft" ~ e ~ ev ~ c ~ t => EFillLeft(e, ev, c, t) }
//     | "liftRight" ~ expr3 ~ expr3 ~ expr3 ~ expr3 ^^
//         { case "liftRight" ~ e ~ ev ~ c ~ t => ELiftRight(e, ev, c, t) }
//     | "fillRight" ~ expr3 ~ expr3 ~ expr3 ~ expr3 ^^
//         { case "fillRight" ~ e ~ ev ~ c ~ t => EFillRight(e, ev, c, t) }
//     | expr3
//   )

//   lazy val expr3: PackratParser[Expr] = (
//       expr3 ~ expr4 ^^
//         { case e ~ f => EApp(e, f) }
//     | expr4
//   )

//   lazy val expr4: PackratParser[Expr] = (
//       "Unit" ^^^ EUnit
//     | "Type" ^^^ EType
//     | "empty" ^^^ EEmpty
//     | "tt" ^^^ ETt
//     | "Cat" ^^^ ECat
//     | trExpr
//     | nstExpr
//     | expr4 ~ ".1" ^^ 
//         { case e ~ ".1" => EFst(e) }
//     | expr4 ~ ".2" ^^ 
//         { case e ~ ".2" => ESnd(e) }
//     | ident ^^ { EVar(_) }
//     | "(" ~ expr ~ ")" ^^
//         { case "(" ~ e ~ ")" => e }
//   )

//   lazy val trExpr: PackratParser[Expr] = (
//       "pt" ~ expr4 ^^
//         { case "pt" ~ e => EPt(e) }
//     | "lf" ^^^ ELf
//     | "nd" ~ expr4 ~ trExpr ^^
//         { case "nd" ~ e ~ sh => ENd(e, sh) }
//     | "(" ~ trExpr ~ ")" ^^
//         { case "(" ~ te ~ ")" => te }
//   )

//   lazy val nstExpr : PackratParser[Expr] = (
//       "dot" ~ expr4 ^^
//         { case "dot" ~ e => EDot(e) }
//     | "box" ~ expr4 ~ trExpr ^^
//         { case "box" ~ e ~ cn => EBox(e, cn) }
//   )

//   lazy val complexExpr: PackratParser[Expr] = (
//       "[" ~ nstExpr ~ "]>>" ~ complexExpr ^^
//         { case "[" ~ n ~ "]>>" ~ c => ETl(n, c) }
//     | "[" ~ nstExpr ~ "]" ^^
//         { case "[" ~ n ~ "]" => EHd(n) }
//   )

//   lazy val addrExpr : PackratParser[Addr] = (
//       addrExpr1 ~ "::" ~ addrExpr ^^
//         { case a ~ "::" ~ b => ACons(a, b) }
//     | addrExpr1
//   )

//   lazy val addrExpr1 : PackratParser[Addr] = (
//       "#" ^^^ AUnit
//     | "nil" ^^^ ANil
//     | "(" ~ addrExpr ~ ")" ^^ 
//         { case "(" ~ a ~ ")" => a }
//   )

//   lazy val pattern: PackratParser[Patt] = (
//       pattern1 ~ "," ~ pattern ^^
//         { case p ~ "," ~ q => PPair(p, q) }
//     | pattern1
//   )

//   lazy val pattern1: PackratParser[Patt] = (
//       "_" ^^^ Punit
//     | ident ^^ { PVar(_) }
//     | "(" ~ pattern ~ ")" ^^ 
//         { case "(" ~ p ~ ")" => p }
//   )

//   lazy val decl: PackratParser[Decl] = (
//       "let" ~ pattern ~ ":" ~ expr ~ "=" ~ expr ^^ 
//         { case "let" ~ p ~ ":" ~ e ~ "=" ~ f => Def(p, e, f) }
//     | "letrec" ~ pattern ~ ":" ~ expr ~ "=" ~ expr ^^ 
//         { case "letrec" ~ p ~ ":" ~ e ~ "=" ~ f => Drec(p, e, f) }
//   )

//   override protected val whiteSpace = """(\s|--.*)+""".r

// }
