/**
  * Main.scala - Main entry point for the typechecker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import scala.io.Source._

import scalaz.-\/
import scalaz.\/-

object Main {

  def main(args: Array[String]) = {

    import OpetopicParser._
    import OpetopicTypeChecker._
    import PrettyPrinter._

    if (args.length != 1) {

      println("Usage: opetopictt <filename>")

    } else {

      val rawLines : String = 
        fromFile(args(0)).getLines.mkString("\n")

      val lines: List[String] = 
        LineParser.parseAll(LineParser.unit, rawLines) match {
          case LineParser.Success(lines, _) => lines
          case err => { println("Failed to join lines: " ++ err.toString) ; Nil }
        }

      def getPat(d: Decl) : Patt =
        d match {
          case Def(p, _, _) => p
          case Drec(p, _, _) => p
        }

      def checkLines(rho: Rho, gma: Gamma, lns: List[String]) : Gamma = 
        lns match {
          case Nil => gma
          case l :: ls => {

            parseAll(phrase(decl), l) match {
              case Success(d, _) => {
                println("Parsed declaration: " ++ prettyPrint(getPat(d)))

                checkD(rho, gma, d) match {
                  case \/-(g) => {
                    println("Typechecked declaration: " ++ prettyPrint(getPat(d)))
                    checkLines(UpDec(rho, d), g, ls)
                  }
                  case -\/(str) => {
                    println("Type check error: " ++ str)
                    Nil
                  }
                }

              }
              case err => {
                println("Parse error: " ++ err.toString)
                Nil
              }
            }

          }
        }

      val result = checkLines(RNil, Nil, lines)

    }
  }
}
