/**
  * SimplePPrint.scala - Another attempt at pretty printing trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.pprint

import opetopic._
import syntax.tree._
import syntax.nesting._

object SimplePPrint {

  sealed trait Token
  case class Literal(s: String) extends Token
  case class TokenString(ts: List[Token]) extends Token

  trait Tokenizeable[A] {
    def tokenize(a: A) : List[Token]
  }

  implicit object StringTokenizeable extends Tokenizeable[String] {
    def tokenize(s: String) = List(Literal(s))
  }

  implicit object IntTokenizeable extends Tokenizeable[Int] {
    def tokenize(i: Int) = List(Literal(i.toString))
  }

  implicit def treeTokenizeable[A, N <: Nat](implicit t: Tokenizeable[A]) : Tokenizeable[Tree[A, N]] = 
    new Tokenizeable[Tree[A, N]] {
      def tokenize(tr: Tree[A, N]) = 
        tokenizeTree(tr.dim)(tr)
    }

  implicit def nestingTokenizeable[A, N <: Nat](implicit t: Tokenizeable[A]) : Tokenizeable[Nesting[A, N]] = 
    new Tokenizeable[Nesting[A, N]] {
      def tokenize(nst: Nesting[A, N]) = 
        tokenizeNesting(nst)
    }

  @natElim
  def tokenizeTree[A, N <: Nat](n: N)(tr: Tree[A, N])(implicit t: Tokenizeable[A]) : List[Token] = {
    case (Z, Pt(a)) => 
      List(Literal("pt"), TokenString(t.tokenize(a)))
    case (S(p: P), Leaf(_)) => 
      List(Literal("leaf"))
    case (S(p: P), Node(a, sh)) => 
      List(Literal("node"), TokenString(t.tokenize(a)), TokenString(tokenizeTree(p)(sh)))

  }

  def tokenizeNesting[A, N <: Nat](nst: Nesting[A, N])(implicit t: Tokenizeable[A]) : List[Token] = 
    nst match {
      case Obj(a) => 
        List(Literal("obj"), TokenString(t.tokenize(a)))
      case Dot(a, _) =>
        List(Literal("dot"), TokenString(t.tokenize(a)))
      case Box(a, cn) =>
        List(Literal("box"), TokenString(tokenizeTree(cn.dim)(cn)))
    }

  def simpleprint[A](a: A)(implicit t: Tokenizeable[A]) : String = 
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

  def pprint[A](a: A, w: Int = 80)(implicit t: Tokenizeable[A]) : String = 
    printLines(stringify(w, 0, t.tokenize(a)))

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
