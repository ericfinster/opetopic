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
      printTest

    } else {

      val lines : String = 
        fromFile(args(0)).mkString

      parseAll(phrase(expr), lines) match {
        case Success(e, _) => {

          println("Parsing succesful, now typechecking ...")

          import scalaz.-\/
          import scalaz.\/-

          check(RNil, Nil, e, Unt) match {
            case -\/(str) => println("Failure: " ++ str)
            case \/-(()) => println("Success!")
          }

        }
        case err => println(err.toString)
      }

    }

  }

  def printTest: Unit = {

    println("Running printTest ...\n")

    import opetopic.pprint.OpetopicTTPrinter._
    import opetopic.Examples._

    println(pprintComplex(fredComplex))

  }

}
