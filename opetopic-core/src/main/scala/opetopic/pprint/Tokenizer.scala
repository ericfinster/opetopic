/**
  * Tokenizer.scala - A Trait for types which can be tokenized
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.pprint

import opetopic._
import syntax.tree._
import syntax.nesting._

trait Tokenizer[A] {
  def tokenize(a: A) : List[Token]
}

object Tokenizer {

  implicit object StringTokenizer extends Tokenizer[String] {
    def tokenize(s: String) = List(Literal(s))
  }

  implicit object IntTokenizer extends Tokenizer[Int] {
    def tokenize(i: Int) = List(Literal(i.toString))
  }

  implicit def treeTokenizer[A, N <: Nat](implicit t: Tokenizer[A]) : Tokenizer[Tree[A, N]] = 
    new Tokenizer[Tree[A, N]] {
      def tokenize(tr: Tree[A, N]) = 
        tokenizeTree(tr.dim)(tr)
    }

  implicit def nestingTokenizer[A, N <: Nat](implicit t: Tokenizer[A]) : Tokenizer[Nesting[A, N]] = 
    new Tokenizer[Nesting[A, N]] {
      def tokenize(nst: Nesting[A, N]) = 
        tokenizeNesting(nst)
    }

  @natElim
  def tokenizeTree[A, N <: Nat](n: N)(tr: Tree[A, N])(implicit t: Tokenizer[A]) : List[Token] = {
    case (Z, Pt(a)) => 
      List(Literal("pt"), TokenString(t.tokenize(a)))
    case (S(p: P), Leaf(_)) => 
      List(Literal("leaf"))
    case (S(p: P), Node(a, sh)) => 
      List(Literal("node"), TokenString(t.tokenize(a)), TokenString(tokenizeTree(p)(sh)))

  }

  def tokenizeNesting[A, N <: Nat](nst: Nesting[A, N])(implicit t: Tokenizer[A]) : List[Token] = 
    nst match {
      case Obj(a) => 
        List(Literal("obj"), TokenString(t.tokenize(a)))
      case Dot(a, _) =>
        List(Literal("dot"), TokenString(t.tokenize(a)))
      case Box(a, cn) =>
        List(Literal("box"), TokenString(tokenizeTree(cn.dim)(cn)))
    }


}

trait IndexedTokenizer[A[_ <: Nat]] {
  def apply[N <: Nat](n: N) : Tokenizer[A[N]]
}

object IndexedTokenizer {

  implicit object ConstIntTokenizer extends IndexedTokenizer[ConstInt] {
    def apply[N <: Nat](n: N) = Tokenizer.IntTokenizer
  }

}
