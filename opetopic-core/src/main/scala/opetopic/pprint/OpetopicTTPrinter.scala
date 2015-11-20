/**
  * OpetopicTTPrinter.scala - A Pretty Printer for OpetopicTT Output
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.pprint

import opetopic._
import syntax.tree._
import syntax.nesting._
import syntax.complex._

object OpetopicTTPrinter {

  import Token._
  import Tokenizer._

  def pprintTree[A, N <: Nat](tr: Tree[A, N])(implicit t: Tokenizer[A]) : String = 
    pprint(tr)

  def pprintNesting[A, N <: Nat](nst: Nesting[A, N])(implicit t: Tokenizer[A]) : String = 
    pprint(nst)

  @natElim
  def pprintComplexLines[A[_ <: Nat], N <: Nat](n: N)(c: Complex[A, N])(implicit it: IndexedTokenizer[A]) : List[String] = {
    case (Z, Complex(_, hd)) => List(pprint(hd, o = 2)(nestingTokenizer(it(Z))))
    case (S(p), Complex(tl, hd)) => pprintComplexLines(p)(tl) ++ List(pprint(hd, o = 2)(nestingTokenizer(it(S(p)))))
  }

  def pprintComplex[A[_ <: Nat], N <: Nat](c: Complex[A, N])(implicit it: IndexedTokenizer[A]) : String = 
    pprintComplexLines(c.dim)(c).mkString("[\n\n", "\n\n]>> [\n\n", "\n\n]")

}
