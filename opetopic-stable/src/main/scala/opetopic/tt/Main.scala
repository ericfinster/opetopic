/**
  * Main.scala - Main Module for MiniTT in Scala
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import scala.io.Source._

import fastparse.core.Parsed.Success
import fastparse.core.Parsed.Failure

import opetopic.mtl._

import Parser._
import Lexer._
import TypeChecker._

object Main {

  def main(args: Array[String]) : Unit = {

    if (args.length != 1) {

      println("Usage: opetopictt <filename>")

    } else {

      val lines : String = 
        fromFile(args(0)).mkString

      program.parse(lines) match {
        case Success(expr, _) => {

          println("Parsing successful.")
          check(RNil, Nil, expr, Unt) match {
            case Xor.Right(_) => println("Typechecking successful.")
            case Xor.Left(msg) => println("Failure: " + msg)
          }


        }
        case f @ Failure(_, _, e) => {
          println("Failure: " + f.msg)
          println(e.traced.trace)
        }
      }

    }

  }

}
