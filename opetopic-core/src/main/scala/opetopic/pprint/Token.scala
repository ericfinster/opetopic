/**
  * Token.scala - Pretty Printing Tokens
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.pprint

sealed trait Token
case class Phrase(seq: Token*) extends Token
case class Literal(str: String) extends Token 
case class Delim(ld: String, t: Token, rd: String) extends Token

object Token {

  def printToken(t : Token) : String = 
    t match {
      case Literal(str) => str
      case Phrase(seq @ _*) => seq.map(printToken(_)).mkString(" ")
      case Delim(ld, t, rd) => ld + printToken(t) + rd
    }

  // def leftPrefix(t: Token, p: String) : Token = 
  //   t match {
  //     case Literal(str) => Literal(p + str)
  //     case Phrase(ts @ _*) => 
  //       if (ts.length > 0)
  //         Phrase((leftPrefix(ts.head, p) +: ts.tail) : _*)
  //       else t
  //   }

  // def rightPostfix(t: Token, p: String) : Token = 
  //   t match {
  //     case Literal(str) => Literal(str + p)
  //     case Phrase(ts @ _*) => 
  //       if (ts.length > 0)
  //         Phrase((ts.tail :+ rightPostfix(ts.head, p)) : _*)
  //       else t
  //   }

  // def parenthesize(t: Token) : Token = 
  //   rightPostfix(leftPrefix(t, "("), ")")

  // def printLines(w: Int, k: Int, t: Token) : List[(Int, String)] = 
  //   t match {
  //     case Literal(str) => List((k, str))
  //     case Phrase(l : Literal, ts @ _*) => {
  //       val remChars = w - k
  //       val tsStr = (ts map printToken).mkString(" ")

  //       List()

  //     }
  //     case Phrase(p: Phrase, ts @ _*) => {
  //       val pp = parenthesize(p)

  //       List()
  //     }
  //   }


  // def stringify(w: Int, k: Int, tl: List[Token]) : List[(Int, String)] = 
  //   tl match {
  //     case Nil => List()
  //     case Literal(s) :: ts => {
  //       val remChars = w - k
  //       val thisString = printTokens(ts)

  //       if (s.length + 1 + thisString.length > remChars) {
  //         (k, s) :: stringify(w, k + 2, ts)
  //       } else {
  //         List((k, s ++ " " ++ thisString))
  //       }
  //     }
  //     case TokenString(s) :: ts => {
  //       val ps = if (s.length > 1) parenthesize(s) else s
  //       stringify(w, k, ps) ++ stringify(w, k, ts)
  //     }
  //   }

}
