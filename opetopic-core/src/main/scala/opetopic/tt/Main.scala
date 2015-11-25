/**
  * Main.scala - Main entry point for the typechecker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import scala.io.Source._

object Main {

  def main(args: Array[String]) = {

    import OpetopicParser._
    import OpetopicTypeChecker._

    if (args.length != 1) {

      println("Usage: opetopictt <filename>")

    } else {

      val rawLines : String = 
        fromFile(args(0)).getLines.mkString("\n")

      LineParser.parseAll(LineParser.unit, rawLines) match {
        case LineParser.Success(lines, _) => 
          for {
            l <- lines
          } {

            println("Parsing line: " ++ l)

            parseAll(phrase(decl), l) match {
              case Success(d @ Def(p, _, _), _) => {
                println("Parsed declaration: " ++ p.toString)
              }
              case Success(d @ Drec(p, _, _), _) => {
                println("Parsed recursice declaration: " ++ p.toString)
              }
              case err => println(err.toString)
            }

          }
        case err => println("Failed to join lines: " ++ err)
      }

    }

  }

}
