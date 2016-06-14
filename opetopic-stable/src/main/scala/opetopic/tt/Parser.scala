/**
  * Parser.scala - Scala MiniTT Parser
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import fastparse.WhitespaceApi

import opetopic._

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

  // Expr

  val expr: Parser[Expr] = 
    P( expr1 ~ ( "," ~ expr1 ).rep(1).? ).map({
      case (e, None) => e
      case (e, Some(s)) => s.foldLeft(e)(EPair(_, _))
    })

  // Expr1

  val expr1: Parser[Expr] = 
    P( constant | lambda | pi | sigma | arrow | product | fst | snd | letdec | expr2 )

  val lambda: Parser[Expr] = 
    P( "\\" ~ pattern1 ~ "." ~ expr ).map({ 
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

  val arrow: Parser[Expr] = 
    P( expr2 ~ "->" ~ expr ).map({
      case (e, f) => EPi(Punit, e, f)
    })

  val product: Parser[Expr] = 
    P( expr2 ~ "*" ~ expr ).map({
      case (e, f) => EPi(Punit, e, f)
    })

  val fst: Parser[Expr] = 
    P( "fst" ~ expr ).map(EFst(_))

  val snd: Parser[Expr] = 
    P( "snd" ~ expr ).map(ESnd(_))

  val letdec: Parser[Expr] = 
    P( decl ~ ";" ~/ expr ).map({
      case (d, e) => EDec(d, e)
    })

  // Constants

  val constants: Map[String, Expr] = Map(
    "Unit" -> EUnit,
    "Type" -> EType,
    "tt" -> ETt,
    "empty" -> EEmpty,
    "Cat" -> ECat
  )

  val constant: Parser[Expr] = 
    P( StringIn(constants.keys.toList : _*).!.map(constants(_)) )

  // Expr2

  val expr2: Parser[Expr] = 
    P( obj | cell |
      isLeftExt | isRightExt |
      refl | drop | comp | fill |
      liftLeft | liftRight | fillLeft | fillRight |
      expr3 )

  val obj: Parser[Expr] = 
    P( "Obj" ~ expr4 ).map(EObj(_))

  val cell: Parser[Expr] = 
    P( "Cell" ~ expr4 ~ tree ~ expr4 ).map({
      case (c, s, t) => ECell(c, s, t)
    })

  val isLeftExt: Parser[Expr] = 
    P( "isLeftExt" ~ expr4 ).map(EIsLeftExt(_))

  val isRightExt: Parser[Expr] = 
    P( "isRightExt" ~ expr4 ~ address ).map({
      case (e, a) => EIsRightExt(e, a)
    })

  val refl: Parser[Expr] = 
    P( "refl" ~ expr4 ~ expr4 ).map({
      case (e, f) => ERefl(e, f)
    })

  val drop: Parser[Expr] = 
    P( "drop" ~ expr4 ~ expr4 ).map({
      case (e, f) => EDrop(e, f)
    })

  val comp: Parser[Expr] = 
    P( "comp" ~ expr4 ~ tree ).map({
      case (e, t) => EComp(e, t)
    })

  val fill: Parser[Expr] = 
    P( "fill" ~ expr4 ~ tree ).map({
      case (e, t) => EFill(e, t)
    })

  val liftLeft: Parser[Expr] = 
    P( "liftLeft" ~ expr4 ~ expr4 ~ expr4 ~ expr4 ).map({
      case (cat, ev, cl, tgt) => ELiftLeft(cat, ev, cl, tgt)
    })

  val liftRight: Parser[Expr] = 
    P( "liftRight" ~ expr4 ~ expr4 ~ expr4 ~ expr4 ).map({
      case (cat, ev, cl, tgt) => ELiftRight(cat, ev, cl, tgt)
    })

  val fillLeft: Parser[Expr] = 
    P( "fillLeft" ~ expr4 ~ expr4 ~ expr4 ~ expr4 ).map({
      case (cat, ev, cl, tgt) => EFillLeft(cat, ev, cl, tgt)
    })

  val fillRight: Parser[Expr] = 
    P( "fillRight" ~ expr4 ~ expr4 ~ expr4 ~ expr4 ).map({
      case (cat, ev, cl, tgt) => EFillRight(cat, ev, cl, tgt)
    })

  // Expr3

  val expr3: Parser[Expr] =
    P( expr4.rep(1) ).map({
      case exprs => {

        def mkApp(ex: Expr, seq: Seq[Expr]): Expr = 
          if (seq.length > 0) 
            mkApp(EApp(ex, seq.head), seq.tail)
          else ex

        if (exprs.length > 1) {
          mkApp(exprs.head, exprs.tail)
        } else exprs.head

      }
    })

  // Expr4

  val expr4: Parser[Expr] = 
    P( Lexer.ident.map(EVar(_)) | "(" ~ expr ~ ")" )

  //============================================================================================
  // TREES, NESTINGS AND COMPLEXES
  //

  val tree: Parser[Expr] = 
    P( "lf".!.map(_ => ELf) | ("nd" ~ (tree | expr4) ~ tree).map({ case (e, s) => ENd(e, s) }) | ("(" ~ tree ~ ")") )

  // val nesting: Parser[Expr] = 
  //   P( ("dot" ~ expr4).map({ case e => EDot(e) }) | 
  //      ("box" ~ expr4 ~ tree).map({ case (e, t) => EBox(e, t) }) )

  // val complex: Parser[Expr] = 
  //   P( ("[" ~ nesting ~ "]>>" ~ complex).map({ case (n, c) => ETl(n, c) }) |
  //      ("[" ~ nesting ~ "]").map(EHd(_))
  //   )

  //============================================================================================
  // ADDRESSES
  //

  val address: Parser[Addr] = 
    P( address1 ~ ("::" ~ address).rep(1).? ).map({
      case (a, None) => a
      case (a, Some(as)) => as.foldLeft(a)(ACons(_, _))
    })

  val address1: Parser[Addr] = 
    P( "nil".!.map(_ => ANil) |
       ("(" ~ address ~ ")")
    )

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
    P( simpleDecl | recursiveDecl )

  val simpleDecl: Parser[Decl] = 
    P( "let" ~/ pattern ~ ":" ~ expr ~ "=" ~ expr ).map({
      case (p, e, f) => Def(p, e, f)
    })

  val recursiveDecl: Parser[Decl] = 
    P( "letrec" ~/ pattern ~ ":" ~ expr ~ "=" ~ expr ).map({
      case (p, e, f) => Drec(p, e, f)
    })

}
