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
    case (Z, Pt(a)) => Phrase(Literal("pt"), a.parenthesize)
    case (S(p: P), Leaf(_)) => Literal("leaf")
    case (S(p: P), Node(a, sh)) => Phrase(Literal("node"), a.parenthesize, tokenizeTree(p)(sh).parenthesize)
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
      case Obj(a) => Phrase(Literal("obj"), a.parenthesize)
      case Dot(a, _) => Phrase(Literal("dot"), a.parenthesize)
      case Box(a, cn) => Phrase(Literal("box"), a.parenthesize, cn.parenthesize)
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
  def tokenizeSuite[A[_ <: Nat], N <: Nat](n: N)(
    s: Suite[A, N], dl: String, dr: String
  )(implicit t: IndexedTokenizer[A]) : Token = {
    case (Z, SNil(), dl, dr) => Literal("")
    case (S(p), tl >> hd, dl, dr) => 
      Phrase(tokenizeSuite(p)(tl, dl, dr), Delim(dl, t(p).tokenize(hd), dr))
  }

  //============================================================================================
  // OPS
  //

  implicit class TokenizerOps[A](a: A)(implicit t: Tokenizer[A]) {

    def tokenize : Token = t.tokenize(a)

    def parenthesize : Token = 
      tokenize.parenthesize

    def pprint : String = Token.printToken(tokenize)

  }

}

trait IndexedTokenizer[A[_ <: Nat]] {
  def apply[N <: Nat](n: N) : Tokenizer[A[N]]
}

object IndexedTokenizer {

  implicit object ConstIntTokenizer extends IndexedTokenizer[ConstInt] {
    def apply[N <: Nat](n: N) = Tokenizer.IntTokenizer
  }

  implicit object ConstStringTokenizer extends IndexedTokenizer[ConstString] {
    def apply[N <: Nat](n: N) = Tokenizer.StringTokenizer
  }

  implicit def toNestingTokenizer[A[_ <: Nat]](t: IndexedTokenizer[A]) 
      : IndexedTokenizer[Lambda[`N <: Nat` => Nesting[A[N], N]]] = 
    new IndexedTokenizer[Lambda[`N <: Nat` => Nesting[A[N], N]]] {
      def apply[N <: Nat](n: N) : Tokenizer[Nesting[A[N], N]] = 
        Tokenizer.nestingTokenizer(t(n))
    }

}
