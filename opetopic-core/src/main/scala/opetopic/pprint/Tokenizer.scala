/**
  * Tokenizer.scala - Tokenizer Type Class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.pprint

import opetopic._

trait Tokenizer[A] {
  def tokenize(a: A) : Token
}

object Tokenizer {

  //============================================================================================
  // INSTANCES
  //

  implicit object StringTokenizer extends Tokenizer[String] {
    def tokenize(s: String) = Literal(s)
  }

  implicit object IntTokenizer extends Tokenizer[Int] {
    def tokenize(i: Int) = Literal(i.toString)
  }

  //============================================================================================
  // TREES INSTANCE
  //

  @natElim
  def tokenizeTree[A, N <: Nat](n: N)(tr: Tree[A, N])(implicit t: Tokenizer[A]) : Token = {
    case (Z, Pt(a)) => Delim("(", Phrase(Literal("pt"), a.tokenize), ")")
    case (S(p: P), Leaf(_)) => Literal("leaf")
    case (S(p: P), Node(a, sh)) => Delim("(", Phrase(Literal("node"), a.tokenize, tokenizeTree(p)(sh)), ")")
  }


  implicit def treeTokenizer[A, N <: Nat](implicit t: Tokenizer[A]) : Tokenizer[Tree[A, N]] = 
    new Tokenizer[Tree[A, N]] {
      def tokenize(tr: Tree[A, N]) = 
        tokenizeTree(tr.dim)(tr)
    }

  //============================================================================================
  // NESTING INSTANCE
  //

  def tokenizeNesting[A, N <: Nat](nst: Nesting[A, N])(implicit t: Tokenizer[A]) : Token = 
    nst match {
      case Obj(a) => Delim("(", Phrase(Literal("obj"), a.tokenize), ")")
      case Dot(a, _) => Delim("(", Phrase(Literal("dot"), a.tokenize), ")")
      case Box(a, cn) => Delim("(", Phrase(Literal("box"), a.tokenize, cn.tokenize), ")")
    }

  implicit def nestingTokenizer[A, N <: Nat](implicit t: Tokenizer[A]) : Tokenizer[Nesting[A, N]] = 
    new Tokenizer[Nesting[A, N]] {
      def tokenize(nst: Nesting[A, N]) = 
        tokenizeNesting(nst)
    }

  //============================================================================================
  // SUITE TOKENIZER
  //

  @natElim
  def tokenizeSuite[A[_ <: Nat], N <: Nat](n: N)(s: Suite[A, N], dl: String, dr: String)(implicit t: IndexedTokenizer[A]) : Token = {
    case (Z, SNil(), dl, dr) => Phrase()
    case (S(p), tl >> hd, dl, dr) => Phrase(tokenizeSuite(p)(tl))
  }

  //============================================================================================
  // OPS
  //

  implicit class TokenizerOps[A](a: A)(implicit t: Tokenizer[A]) {
    def tokenize : Token = t.tokenize(a)
    def pprint : String = Token.printToken(tokenize)
  }

}
