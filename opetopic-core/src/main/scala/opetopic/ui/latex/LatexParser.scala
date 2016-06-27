/**
  * LatexParser.scala - A Simple Latex Parser
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui.latex

import fastparse.all._

case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
  def apply(t: T) = f(t)
  override def toString() = name
}

object LatexParser {

  protected val escapeNames: Seq[String] = helper.Escape.names.toSeq
  protected val unaryNames: Seq[String] = helper.Unary.names.toSeq
  protected val unaryWithOptionNames: Seq[String] = helper.UnaryWithOption.names.toSeq
  protected val binaryNames: Seq[String] = helper.Binary.names.toSeq
  protected val styleNames: Seq[String] = helper.Style.names.toSeq

  protected def translateCharLiteral(matched: String) : String = matched
  protected def translateSpaces(matched: String) : String = " "
  protected def translateSpacesMultiNewLine(matched: String) : String = {
    val newline = "\n" // sys.props("line.separator")
    newline + newline
  }

  protected def translateEscape(name: String): String = helper.Escape.translate(name)
  protected def translateUnary(command: String, param: String) = helper.Unary.translate(command, param)
  protected def translateUnaryWithOption(command: String, option: String, param: String) = helper.UnaryWithOption.translate(command, option, param)
  protected def translateBinary(command: String, param1: String, param2: String) = helper.Binary.translate(command, param1, param2)
  protected def translateStyle(command: String, text: String) = helper.Style.translate(command, text)
  protected def translateUnknownCommand(matched: String) = matched

  val SimpleWhitespace = NamedFunction(" \r\t".contains(_: Char), "SimpleWhitespace")
  val Whitespace = NamedFunction((c: Char) => SimpleWhitespace(c) || c == "\n", "Whitespace")
  val NonSpacePunctuation = NamedFunction("$^_~{}\\".contains(_: Char), "NonSpacePunctuation")
  val Punctuation = NamedFunction((c : Char) => Whitespace(c) || NonSpacePunctuation(c), "Punctuation")

  val Input: Parser[String] = 
    P( (Text ~ End) | P( End ).map(_ => "") )

  val Text: Parser[String] = 
    P( (Expression | WhiteSpaces) ~ Text.? ).map({
      case (a, o) => a + o.getOrElse("")
    })

  val Expression: Parser[String] = 
    P( CharLiteral | Group | Command )

  val Group: Parser[String] = 
    P( "{" ~ Text.! ~ "}" )

  val CharLiteral: Parser[String] = 
    P( CharPred(! Punctuation(_)).! ).map(translateCharLiteral(_))

  val WhiteSpaces: Parser[String] = 
    P( Spaces | SpacesMultiNewLine )

  val NewLineWhitespace: Parser[String] = 
    P( "\n".! ~ CharsWhile(SimpleWhitespace).! ).map({
      case (a, b) => a + b
    })

  val Spaces: Parser[String] = 
    P( CharsWhile(SimpleWhitespace).! ~ NewLineWhitespace.? ).map({
      case (a, o) => translateSpaces(a + o.getOrElse(""))
    })

  val SpacesMultiNewLine: Parser[String] = 
    P( CharsWhile(SimpleWhitespace).! ~ NewLineWhitespace.rep(2) ).map({
      case (a, seq) => translateSpacesMultiNewLine(a + seq.mkString)
    })

  val Command: Parser[String] = 
    P( Escape | Unary | UnaryWithOption | Binary | Style | UnknownCommand )

  val Escape: Parser[String] = 
    P( StringIn(escapeNames : _*).! ).map({
      translateEscape(_)
    })

  val Unary: Parser[String] = 
    P( StringIn(unaryNames : _*).! ~ Spaces.? ~ Expression ).map({
      case (cmd, _, prm) => translateUnary(cmd, prm)
    })

  val UnaryWithOption: Parser[String] = 
    P( StringIn(unaryWithOptionNames : _*).! ~ Spaces.? ~ 
      ( CommandOption.! ~ Spaces.? | P( CharPred(_ != '[') ).map(_ => ("", ""))) ~
      Expression
    ).map({
      case (cmd, _, (opt, _), prm) => translateUnaryWithOption(cmd, opt, prm)
    })

  val CommandOption: Parser[String] = 
    P( P( "[]" ).map(_ => "") | "[" ~ CommandOptionText ~ "]" )

  val CommandOptionText: Parser[String] = 
    P( CharPred(_ != '[') ~ (Expression.! | WhiteSpaces.?.!) ~ CommandOptionText.? ).map({
      case (a, None) => a
      case (a, b) => a + b
    })

  val Binary: Parser[String] = 
    P( StringIn(binaryNames : _*).! ~ Spaces.? ~ Expression ~ Spaces.? ~ Expression ).map({
      case (cmd, _, p, _, q) => translateBinary(cmd, p, q)
    })

  val Style: Parser[String] = 
    P( StringIn(styleNames : _*).! ~ Text ).map({
      case (sty, txt) => translateStyle(sty, txt)
    })

  val UnknownCommand = 
    P( "\\" ~ ( CharIn('a' to 'z') | CharIn('A' to 'Z') ).rep.! ~
      ( Spaces.? ~ CommandOption ).?.! ~ Spaces.?
    ).map({ case (cmd, _, _) => translateUnknownCommand("\\" + cmd) })

}
