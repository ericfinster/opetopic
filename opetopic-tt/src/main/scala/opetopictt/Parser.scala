/**
  * Parser.scala - OpetopicTT Parser
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopictt

import fastparse.WhitespaceApi

object Lexer {

  import fastparse.all._

  val comment = P( "--" ~ CharsWhile(_ != '\n', min = 0) )
  val wscomment = P( (CharsWhile(" \t\n".toSet, min = 1) | comment ).rep )

  val letter     = P( lowercase | uppercase )
  val lowercase  = P( CharIn('a' to 'z') )
  val uppercase  = P( CharIn('A' to 'Z') )
  val digit      = P( CharIn('0' to '9') )

  // Right, well, you can do better ...
  val ident: Parser[String] = 
    P( CharsWhile(_.isLetterOrDigit) ).!

}

object WsApi extends WhitespaceApi.Wrapper(Lexer.wscomment)

object Parser {

  import fastparse.noApi._
  import WsApi._

  // Testing

  val program: P[Expr] = 
    P( Lexer.wscomment.? ~ expr ~ End )

  //============================================================================================
  // EXPRESSIONS
  //

  // Constants

  val constants: Map[String, Expr] = Map(
    "Id" -> EId,
    "Mnd" -> EMnd,
    "IndexOf" -> EIndexOf,
    "ConsOf" -> EConsOf,
    "etaI" -> EEtaI,
    "etaE" -> EEtaE,
    "eta" -> EEta,
    "muI" -> EMuI,
    "muE" -> EMuE,
    "mu" -> EMu,
    "SlCn" -> ESlCn,
    "Sl" -> ESl,
    "dot" -> EDot,
    "box" -> EBox,
    "Unit" -> EUnit,
    "tt" -> ETt,
    "Type" -> EType
  )

  val constant: Parser[Expr] = 
    P( StringIn(constants.keys.toList : _*).!.map(constants(_)) )

  // Expr

  val expr: Parser[Expr] = 
    P( lambda | pi | sigma | fst | snd | letdec | phi | kappa | typeOf | expr3 )

  val lambda: Parser[Expr] = 
    P( "\\" ~/ pattern1 ~ "." ~ expr ).map({ 
      case (p, e) => ELam(p, e) 
    })

  val pi: Parser[Expr] = 
    P( "(" ~ pattern1 ~ ":" ~ expr ~ ")" ~ "->" ~/ expr ).map({ 
      case (p, e, f) => EPi(p, e, f) 
    })

  val sigma: Parser[Expr] = 
    P( "(" ~ pattern1 ~ ":" ~ expr ~ ")" ~ "*" ~/ expr ).map({ 
      case (p, e, f) => ESig(p, e, f) 
    })

  val fst: Parser[Expr] = 
    P( "fst" ~/ expr ).map(EFst(_))

  val snd: Parser[Expr] = 
    P( "snd" ~/ expr ).map(ESnd(_))

  val letdec: Parser[Expr] = 
    P( decl ~ ";" ~ expr ).map({
      case (d, e) => EDec(d, e)
    })

  val phi: Parser[Expr] = 
    P( "(" ~ pattern1 ~ ":>" ~ atom ~ atom ~ atom ~ ")" ~ "->" ~/ expr ).map({
      case (p, m, i, c, e) => EPhi(p, m, i, c, e)
    })

  val kappa: Parser[Expr] =
    P( "<" ~ pattern ~ ">" ~/ expr ).map({
      case (p, e) => EKap(p, e)
    })

  // Expr3

  val expr3: Parser[Expr] = 
    P( NoCut(pairing) | NoCut(arrow) | NoCut(product) | NoCut(select) | expr4 )

  val pairing: Parser[Expr] = 
    P( atom ~ "," ~ expr ).map({
      case (e, f) => EPair(e, f)
    })

  val arrow: Parser[Expr] = 
    P( atom ~ "->" ~ expr ).map({
      case (e, f) => EPi(Punit, e, f)
    })

  val product: Parser[Expr] = 
    P( atom ~ "*" ~ expr ).map({
      case (e, f) => EPi(Punit, e, f)
    })

  val select: Parser[Expr] = 
    P( atom ~ "@" ~ expr ).map({
      case (d, p) => ESel(d, p)
    })

  val typeOf: Parser[Expr] = 
    P( "[" ~ expr ~ "]" ).map(ETypeOf(_))

  val atom: Parser[Expr] = 
    P( constant | Lexer.ident.map(EVar(_)) | typeOf | "(" ~ expr ~ ")" )

  // Expr4 - (Applications)

  val expr4: Parser[Expr] =
    P( atom.rep(min = 1) ).map({
      case exprs => {

        // println("Parsed application: " + exprs.toString)

        def mkApp(ex: Expr, seq: Seq[Expr]): Expr = 
          if (seq.length > 0) 
            mkApp(EApp(ex, seq.head), seq.tail)
          else ex

        if (exprs.length > 1) {
          mkApp(exprs.head, exprs.tail)
        } else exprs.head

      }
    })

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
      | Lexer.ident.!.map(PVar(_))
      | "(" ~ pattern ~ ")"
    )

  //============================================================================================
  // DECLARATIONS
  //

  val decl: Parser[Decl] = 
    P( simpleDecl | recursiveDecl | normalize )

  val simpleDecl: Parser[Decl] = 
    P( "let" ~/ pattern ~ ":" ~ expr ~ "=" ~ expr ).map({
      case (p, e, f) => Def(p, e, f)
    })

  val recursiveDecl: Parser[Decl] = 
    P( "letrec" ~/ pattern ~ ":" ~ expr ~ "=" ~ expr ).map({
      case (p, e, f) => Drec(p, e, f)
    })

  val normalize: Parser[Decl] = 
    P( "normalize" ~/ expr ).map(Dnorm(_))

}
