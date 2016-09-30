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
      case _ => super.tokenizeExpT(e)
    }

  override def tokenizePTeleT(a: PTeleT): Token =
    a match {
      case PTele(patt, exp) => Delim("(", Phrase(patt.tokenize, Literal(":"), exp.tokenize), ")")
    }

  override def tokenizeListNstT(a: List[NstT]): Token =
    a match {
      case Nil => Phrase()
      case x :: Nil => Phrase(x.tokenize)
      case x :: xs => Phrase(x.tokenize, Literal("||"), xs.tokenize)
    }

}
