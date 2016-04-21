/**
  * Main.scala - Main Module for MiniTT in Scala
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.tt

import scala.io.Source._

import scalaz.\/
import scalaz.-\/
import scalaz.\/-

import OTTParser._
import OTTTypeChecker._

object Main {

  def main(args: Array[String]) : Unit = {

    if (args.length != 1) {

      println("Usage: opetopictt <filename>")

    } else {

      val lines : String = 
        fromFile(args(0)).mkString

      parseAll(phrase(expr), lines) match {
        case Success(e, _) => {

          println("Parsing succesful, now typechecking ...")

          check(RNil, Nil, e, Unt) match {
            case -\/(msg) => println("Typechecking error: " + msg)
            case \/-(()) => println("Success")
          }
        }
        case err => println(err.toString)
      }
    }

  }

}
