/**
  * Main.scala - Main Module for OpetopicTT in Scala
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mtt

import java.io._

import opetopic._
import mtl._

import MttSyntax._
// import TypeChecker._

object Main {

  // import OttPrettyPrinter._

  def main(args: Array[String]) : Unit = {

    if (args.length != 1) {

      println("Usage: opetopicmtt <filename>")

    } else {

    //   val fname = args(0)

    //   println("Typechecking file: " + fname)

    //   val reader = new FileReader(new File(fname))
    //   val lexer = new OttLexer(reader)
    //   val parser = new OttParser
    //   parser.lexer = lexer

    //   parser.parseAll match {
    //     case Right(Module(mid, ds)) => {

    //       println("Checking module: " + mid)

    //       checkDecls(ds).run(TCEnv(Nil, RNil)) match {
    //         case Xor.Left(msg) => println("Typechecking error: " + msg)
    //         case Xor.Right(_) => println("Success!")
    //       }

    //     }
    //     case Right(_) => println("Unknown error")
    //     case Left(s) => println("Parse error: " + s)
    //   }

    //   reader.close

    }

  }

}

