/**
  * PrettyPrinter.scala - PrettyPrinter for OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import opetopic._
import syntax.suite._
import opetopic.pprint._
import IndexedTokenizer._
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

  implicit object AddressTokenizer extends Tokenizer[Addr] {
    def tokenize(a: Addr) : Token = 
      a match {
        case AUnit => Literal("#")
        case ANil => Literal("nil")
        case ACons(a, b) => Phrase(a.tokenize, Literal("::"), b.tokenize)
      }
  }

  implicit object DeclarationTokenizer extends Tokenizer[Decl] {
    def tokenize(d: Decl) : Token =
      d match {
        case Def(p, e, f) => Phrase(Literal("let"), p.tokenize, Literal(":"), ExprTokenizer.tokenize(e), Literal("="), ExprTokenizer.tokenize(f))
        case Drec(p, e, f) => Phrase(Literal("letrec"), p.tokenize, Literal(":"), ExprTokenizer.tokenize(e), Literal("="), ExprTokenizer.tokenize(f))
      }
  }

  implicit object ExprTokenizer extends Tokenizer[Expr] {

    def tokenizeFrame[N <: Nat](c: ExprComplex[N]) : Token = {
      val tl = Suite.tail[NstExpr, N](c)
      val hd = Suite.head[NstExpr, N](c)
      Phrase(tokenizeSuite(tl.length)(tl, "[ ", " ]>>"), Delim("[ ", hd.tokenize, " ]"))
    }

    def tokenizeNchFrame[N <: Nat](s: Suite[NstExpr, N]) : Token = 
      tokenizeSuite(s.length)(s, "[ ", " ]>>")

    def tokenize(e: Expr) : Token =
      e match {
        case EType => Literal("Type")
        case EEmpty => Literal("empty")
        case EUnit => Literal("Unit")
        case ETt => Literal("tt")
        case EVar(id) => Literal(id)
        case ELam(p, e) => Phrase(Delim("\\", p.tokenize, "."), e.tokenize)
        case EPi(p, e, t) => Phrase(Delim("(", Phrase(p.tokenize, Literal(":"), e.tokenize), ")"), Literal("->"), t.tokenize)
        case ESig(p, e, t) => Phrase(Delim("(", Phrase(p.tokenize, Literal(":"), e.tokenize), ")"), Literal("*"), t.tokenize)
        case EPair(e, f) => Phrase(e.tokenize, Literal(","), f.tokenize)
        case EFst(e) => Phrase(e.tokenize, Literal(".1"))
        case ESnd(e) => Phrase(e.tokenize, Literal(".2"))
        case EApp(e, f) => Phrase(e.tokenize, f.parenthesize)
        case EDec(d, e) => Phrase(d.tokenize, Literal(";"), e.tokenize)
        case ERec(_) => Literal("record")
        case EProj(_, _) => Literal("projection")

        case ECat => Literal("Cat")
        case EOb(e) => Phrase(Literal("Obj"), e.parenthesize)
        case EHom(e, c) => Phrase(Literal("Hom"), e.parenthesize, tokenizeFrame(c))
        case ECell(e, c) => Phrase(Literal("Cell"), e.parenthesize, tokenizeFrame(c))

        case EIsLeftExt(e) => Phrase(Literal("isLeftExt"), e.parenthesize)
        case EIsRightExt(e, a) => Phrase(Literal("isRightExt"), e.parenthesize, a.parenthesize)

        case EComp(e, fp, nch) => Phrase(Literal("comp"), e.parenthesize, tokenizeNchFrame(fp), Delim("{ ", nch.tokenize, " }"))
        case EFill(e, fp, nch) => Phrase(Literal("fill"), e.parenthesize, tokenizeNchFrame(fp), Delim("{ ", nch.tokenize, " }"))
        case ELiftLeft(e, ev, c, t) => Phrase(Literal("liftLeft"), e.parenthesize, ev.parenthesize, c.parenthesize, t.parenthesize)
        case EFillLeft(e, ev, c, t) => Phrase(Literal("fillLeft"), e.parenthesize, ev.parenthesize, c.parenthesize, t.parenthesize)
        case ELiftRight(e, ev, c, t) => Phrase(Literal("liftRight"), e.parenthesize, ev.parenthesize, c.parenthesize, t.parenthesize)
        case EFillRight(e, ev, c, t) => Phrase(Literal("fillRight"), e.parenthesize, ev.parenthesize, c.parenthesize, t.parenthesize)

        case EFillIsLeft(e, fp, nch) => Phrase(Literal("fillIsLeft"), e.parenthesize, tokenizeNchFrame(fp), Delim("{", nch.tokenize, "}"))
        case EShellIsLeft(e, ev, src, tgt) => Phrase(Literal("shellIsLeft"), e.parenthesize, ev.parenthesize, src.parenthesize, tgt.parenthesize)
        case EFillLeftIsLeft(e, ev, c, t) => Phrase(Literal("fillLeftIsLeft"), e.parenthesize, ev.parenthesize, c.parenthesize, t.parenthesize)
        case EFillRightIsLeft(e, ev, c, t) => Phrase(Literal("fillRightIsLeft"), e.parenthesize, ev.parenthesize, c.parenthesize, t.parenthesize)
        case EFillLeftIsRight(e, ev, c, t, l, f, fev) => 
          Phrase(Literal("fillLeftIsRight"), e.parenthesize, ev.parenthesize, c.parenthesize, t.parenthesize, l.parenthesize, f.parenthesize, fev.parenthesize)
        case EFillRightIsRight(e, ev, c, t, l, f, fev) => 
          Phrase(Literal("fillRightIsRight"), e.parenthesize, ev.parenthesize, c.parenthesize, t.parenthesize, l.parenthesize, f.parenthesize, fev.parenthesize)

        case EPt(e) => Phrase(Literal("pt"), e.parenthesize)
        case ELf => Literal("lf")
        case ENd(e, t) => Phrase(Literal("nd"), e.parenthesize, t.parenthesize)

      }

  }

  implicit val exprNstTokenizer : IndexedTokenizer[NstExpr] =
    toNestingTokenizer(new IndexedTokenizer[ConstExpr] {
      def apply[N <: Nat](n: N) = ExprTokenizer
    })

}
