/**
  * PrettyPrinter.scala - PrettyPrinter for OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopictt.pprint

import opetopictt._
import Tokenizer._
import Token._

object PrettyPrinter {

  implicit object PatternTokenizer extends Tokenizer[Patt] {
    def tokenize(p: Patt) : Token = 
      p match {
        case Punit => Literal("_")
        case PVar(id) => Literal(id)
        case PPair(p: Patt, q: Patt) => Phrase(p.tokenize, Literal(","), q.tokenize)
      }
  }

  implicit object DeclarationTokenizer extends Tokenizer[Decl] {
    def tokenize(d: Decl) : Token =
      d match {
        case Def(p, e, f) => Phrase(Literal("let"), p.tokenize, Literal(":"), ExprTokenizer.tokenize(e), Literal("="), ExprTokenizer.tokenize(f))
        case Drec(p, e, f) => Phrase(Literal("letrec"), p.tokenize, Literal(":"), ExprTokenizer.tokenize(e), Literal("="), ExprTokenizer.tokenize(f))
        case Dnorm(e) => Phrase(Literal("normalize"), e.tokenize)
      }
  }

  implicit object ExprTokenizer extends Tokenizer[Expr] {

    def tokenize(e: Expr) : Token = 
      e match {
        case EType => Literal("Type")
        case EUnit => Literal("Unit")
        case ETt => Literal("tt")

        case EId => Literal("Id")

        case EMnd => Literal("Mnd")
        case EIndexOf => Literal("IndexOf")
        case EConsOf => Literal("ConsOf")

        case EEta => Literal("eta")
        case EEtaI => Literal("etaI")
        case EEtaE => Literal("etaE")

        case EMu => Literal("mu")
        case EMuI => Literal("muI")
        case EMuE => Literal("muE")

        case ESl => Literal("Sl")
        case ESlCn => Literal("SlCn")
        case EBox => Literal("box")
        case EDot => Literal("dot")

        case EPhi(p, m, i, c, e) => Phrase(Delim("(", Phrase(p.tokenize, Literal(":>"), m.parenthesize, i.parenthesize, c.parenthesize), ")"), Literal("->"), e.tokenize)
        case EKap(p, e) => Phrase(Delim("<", p.tokenize, ">"), e.tokenize)
        case ETypeOf(p) => Phrase(Delim("[", p.tokenize, "]"))
        case ESel(d, p) => Phrase(d.parenthesize, Literal("@"), p.parenthesize)

        case EVar(id) => Literal(id)
        case ELam(p, e) => Phrase(Delim("\\", p.tokenize, "."), e.tokenize)
        case EPi(p, e, t) => Phrase(Delim("(", Phrase(p.tokenize, Literal(":"), e.tokenize), ")"), Literal("->"), t.tokenize)
        case ESig(p, e, t) => Phrase(Delim("(", Phrase(p.tokenize, Literal(":"), e.tokenize), ")"), Literal("*"), t.tokenize)
        case EPair(e, f) => Phrase(e.tokenize, Literal(","), f.tokenize)
        case EFst(e) => Phrase(Literal("fst"), e.tokenize)
        case ESnd(e) => Phrase(Literal("snd"), e.tokenize)
        case EApp(e, f) => Phrase(e.tokenize, f.parenthesize)
        case EDec(d, e) => Phrase(d.tokenize, Literal(";"), e.tokenize)
      }

  }

}
