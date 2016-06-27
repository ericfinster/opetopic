/**
  * Tokenizer.scala - Tokenizer Type Class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopictt.pprint

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
  // OPS
  //

  implicit class TokenizerOps[A](a: A)(implicit t: Tokenizer[A]) {

    def tokenize : Token = t.tokenize(a)

    def parenthesize : Token = 
      tokenize.parenthesize

    def pprint : String = Token.printToken(tokenize)

  }

}

