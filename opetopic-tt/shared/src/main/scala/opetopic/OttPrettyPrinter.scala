/**
  * OttPrettyPrinter.scala - Expression printer for Ott
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ott

import OttSyntax._

object OttPrettyPrinter extends OttPPrint {

  override def tokenizeExpT(e: ExpT): Token =
    e match {
      case EApp(e1, e2) => Phrase(e1.tokenize, e2.parenthesize)
      case EPi(pts, e1) => Phrase(pts.tokenize, Literal("->"), e1.tokenize)
      case ELam(patt, exp) => Phrase(Delim("\\", patt.parenthesize, "."), exp.tokenize)
      case ECell(exp, nsts) => Phrase(Literal("Cell"), exp.parenthesize, Literal("["), nsts.tokenize, Literal("]"))
      case EObj(exp) => Delim("|", exp.tokenize, "|")
      case _ => super.tokenizeExpT(e)
    }

  override def tokenizePTeleT(a: PTeleT): Token =
    a match {
      case PTele(patt, exp) => Delim("(", Phrase(patt.tokenize, Literal(":"), exp.tokenize), ")")
    }

  override def tokenizeTeleT(a: TeleT): Token =
    a match {
      case Tele(patt, exp) => Delim("(", Phrase(patt.tokenize, Literal(":"), exp.tokenize), ")")
    }

  override def tokenizeListTeleT(a: List[TeleT]): Token =
    a match {
      case Nil => Phrase()
      case x :: xs => Phrase(x.tokenize, xs.tokenize)
    }

  override def tokenizeListNstT(a: List[NstT]): Token =
    a match {
      case Nil => Phrase()
      case x :: Nil => Phrase(x.tokenize)
      case x :: xs => Phrase(x.tokenize, Literal("|"), xs.tokenize)
    }

  override def tokenizeDeclT(a: DeclT): Token =
    a match {
      case Def(id, teles, exp, expwhere) => Phrase(id.tokenize, teles.tokenize, Literal(":"), exp.tokenize, Literal("="), expwhere.tokenize)
    }

  override def tokenizeListDeclT(a: List[DeclT]): Token =
    a match {
      case Nil => Phrase()
      case x :: Nil => Phrase(x.tokenize)
      case x :: xs => Phrase(x.tokenize, Literal(";"), xs.tokenize)
    }

  override def tokenizeExpWhereT(a: ExpWhereT): Token =
    a match {
      case Where(exp, decls) => Phrase(exp.tokenize, Literal("where"), Literal("{"), decls.tokenize, Literal("}"))
      case NoWhere(exp) => Phrase(exp.tokenize)
    }

  
}
