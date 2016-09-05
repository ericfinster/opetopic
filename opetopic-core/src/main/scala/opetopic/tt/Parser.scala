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

  // Expr

  val expr: Parser[Expr] = 
    P( constant | lambda | pi | sigma | fst | snd | letdec | expr2 )

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

  // Expr2

  val expr2: Parser[Expr] = 
    P( obj | cell |
      isLeftExt | isRightExt |
      refl | drop | comp | fill |
      liftLeft | liftRight | fillLeft | fillRight |
      expr3 )

  val obj: Parser[Expr] = 
    P( "Ob" ~/ expr ).map(EObj(_))

  val cell: Parser[Expr] = 
    P( "Cell" ~/ expr ~ complex ).map({
      case (c, frm) => ECell(c, frm)
    })

  val isLeftExt: Parser[Expr] = 
    P( "isLeftExt" ~/ expr ).map(EIsLeftExt(_))

  val isRightExt: Parser[Expr] = 
    P( "isRightExt" ~/ expr ~ address ).map({
      case (e, a) => EIsRightExt(e, a)
    })

  val refl: Parser[Expr] = 
    P( "refl" ~/ expr ).map(ERefl(_))

  val drop: Parser[Expr] = 
    P( "drop" ~/ expr ).map(EDrop(_))

  val comp: Parser[Expr] = 
    P( "comp" ~/ exprTree ).map(EComp(_))

  val fill: Parser[Expr] = 
    P( "fill" ~/ exprTree ).map(EFill(_))

  val liftLeft: Parser[Expr] = 
    P( "liftLeft" ~/ expr ~ expr ~ expr ~ expr ).map({
      case (e, ev, cl, tgt) => ELiftLeft(e, ev, cl, tgt)
    })

  val liftRight: Parser[Expr] = 
    P( "liftRight" ~/ expr ~ expr ~ expr ~ expr ).map({
      case (e, ev, cl, tgt) => ELiftRight(e, ev, cl, tgt)
    })

  val fillLeft: Parser[Expr] = 
    P( "fillLeft" ~/ expr ~ expr ~ expr ~ expr ).map({
      case (e, ev, cl, tgt) => EFillLeft(e, ev, cl, tgt)
    })

  val fillRight: Parser[Expr] = 
    P( "fillRight" ~/ expr ~ expr ~ expr ~ expr ).map({
      case (e, ev, cl, tgt) => EFillRight(e, ev, cl, tgt)
    })

  // Expr3

  val expr3: Parser[Expr] = 
    P( NoCut(pairing) | NoCut(arrow) | NoCut(product) | expr4 )

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

  val atom: Parser[Expr] = 
    P( Lexer.ident.map(EVar(_)) | "(" ~ expr ~ ")" )

  // Expr4 - (Applications)

  val expr4: Parser[Expr] =
    P( atom.rep(min = 1) ).map({
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

  //============================================================================================
  // TREES, NESTINGS AND COMPLEXES
  //

  val exprTree: Parser[STree[Expr]] = stree(atom)
    
  def stree[A](ap: Parser[A]): Parser[STree[A]] = 
    P( "lf".!.map(_ => SLeaf) | 
       ("nd" ~ ap ~ stree[STree[A]](stree(ap)) ).map({
         case (a, sh) => SNode(a, sh)
       }) |
      "(" ~ stree(ap) ~ ")"
    )

  val nesting: Parser[SNesting[Expr]] = 
    P( ("dot" ~ atom).map(SDot(_)) | 
       ("box" ~ atom ~ stree(nesting)).map({
         case (e, cn) => SBox(e, cn)
       }) | 
       ( "(" ~ nesting ~ ")" )
    )

  val complex: Parser[SComplex[Expr]] = 
    P( "[" ~ nesting.rep(min = 1, sep = "||") ~ "]" ).map({
      case seq => {
        seq.tail.foldLeft(||(seq.head) : SComplex[Expr])(_ >> _)
      }
    })

  //============================================================================================
  // ADDRESSES
  //

  val address: Parser[SAddr] = 
    P( "{" ~ address.rep(sep = "|") ~ "}" ).map({
      case s => s.toList.map(SDir(_))
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
