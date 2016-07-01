/**
  * Main.scala - Main module for opetopictt
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopictt

import scala.io.Source._

import opetopic.mtl._
import Xor._

import fastparse.core.Parsed.Success
import fastparse.core.Parsed.Failure

import Parser._
import TypeChecker._

object Main {

  def main(args: Array[String]) : Unit = {

    import Examples._
    import ExprGen._

    for {
      t <- exprGen(threecell)
    } { 

      import opetopictt.pprint._
      import Tokenizer._

      println("\nResult: \n\n" + t.pprint + "\n")

    }

  }


  // def main(args: Array[String]) : Unit = {

  //   if (args.length != 1) {

  //     println("Usage: opetopictt <filename>")

  //   } else {

  //     val lines : String = 
  //       fromFile(args(0)).mkString

  //     println("OpetopicTT")
      
  //     program.parse(lines) match {
  //       case Success(expr, _) => {

  //         println("Parsing successful.")
  //         // println(expr.toString)

  //         check(RNil, Nil, expr, Unt) match {
  //           case Right(_) => println("Typechecking successful.")
  //           case Left(msg) => println("Failure: " + msg)
  //         }

  //       }
  //       case f @ Failure(_, _, e) => {
  //         println("Failure: " + f.msg)
  //         println(e.traced.trace)
  //       }
  //     }

  //   }

  // }

}
