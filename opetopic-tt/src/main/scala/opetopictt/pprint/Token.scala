/**
  * Token.scala - Pretty Printing Tokens
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopictt.pprint

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

}
