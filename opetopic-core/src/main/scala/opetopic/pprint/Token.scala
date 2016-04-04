/**
  * Token.scala - Pretty Printing Tokens
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.pprint

sealed trait Token {

  def parenthesize : Token = 
    this match {
      case p: Phrase => Delim("(", p, ")")
      case t => t
    }

}

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

  // Old stuff for doing line truncation ...

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
