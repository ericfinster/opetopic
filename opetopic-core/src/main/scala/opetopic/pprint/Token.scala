/**
  * Token.scala - Routines for Tokens
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.pprint

sealed trait Token
case class Literal(s: String) extends Token
case class TokenString(ts: List[Token]) extends Token

object Token {

  def simpleprint[A](a: A)(implicit t: Tokenizer[A]) : String = 
    t.tokenize(a).map(printToken(_)).mkString(" ")

  def insertLeftParen(tl: List[Token]) : List[Token] = 
    tl match {
      case Nil => Nil
      case Literal(s) :: ts => Literal("(" ++ s) :: ts
      case TokenString(s) :: ts => TokenString(insertLeftParen(s)) :: ts
    }

  def insertRightParen(tl: List[Token]) : List[Token] = 
    tl match {
      case Nil => Nil
      case Literal(s) :: Nil => Literal(s ++ ")") :: Nil
      case TokenString(s) :: Nil => TokenString(insertRightParen(s)) :: Nil
      case t :: ts => t :: insertRightParen(ts)
    }

  def parenthesize(tl: List[Token]) : List[Token] = 
    insertLeftParen(insertRightParen(tl))

  def printToken(t: Token) : String = 
    t match {
      case Literal(s) => s
      case TokenString(ts) => printTokens(ts)
    }

  def printTokens(tl: List[Token]) : String = 
    tl match {
      case Nil => ""
      case t :: Nil => printToken(t)
      case _ => tl.map(printToken(_)).mkString("(", " ", ")")
    }

  def printLines(lines: List[(Int, String)]) : String = 
    (lines map {
      case (i, s) => (" " * i) ++ s
    }).mkString("\n")

  def pprint[A](a: A, w: Int = 120, o: Int = 0)(implicit t: Tokenizer[A]) : String = 
    printLines(stringify(w, o, t.tokenize(a)))

  def stringify(w: Int, k: Int, tl: List[Token]) : List[(Int, String)] = 
    tl match {
      case Nil => List()
      case Literal(s) :: ts => {
        val remChars = w - k
        val thisString = printTokens(ts)

        if (s.length + 1 + thisString.length > remChars) {
          (k, s) :: stringify(w, k + 2, ts)
        } else {
          List((k, s ++ " " ++ thisString))
        }
      }
      case TokenString(s) :: ts => {
        val ps = if (s.length > 1) parenthesize(s) else s
        stringify(w, k, ps) ++ stringify(w, k, ts)
      }
    }

}
