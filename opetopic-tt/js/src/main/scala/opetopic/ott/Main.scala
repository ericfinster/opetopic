/**
  * Main.scala - Main Module for OpetopicTT in Scala
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ott

import scala.scalajs.js.JSApp

import opetopic._
import mtl._

import OttSyntax._
import TypeChecker._

object Main extends JSApp {

  def main: Unit = {
    println("Started opetopic-tt...")

    val test = EPair(EUnit(), EUnit())
    println("Test expression: " ++ test.toString)

  }
  
}
