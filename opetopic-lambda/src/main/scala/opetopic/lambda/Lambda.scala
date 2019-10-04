/**
  * Lambda.scala - An opetopic implementation of planar lambda calculus
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.lambda

import opetopic._
import opetopic.ui._
import opetopic.js._
import opetopic.js.ui._

import JsDomFramework._
import JQuerySemanticUI._

object Lambda {

  def main: Unit = {
    println("Started Lambda Editor")
  }

  // Right. So, main idea is to put an editor in the middle, define an
  // expression langugage, and then write a kind of typechecker like
  // thing which will verify expressions.....

  // We'll want to accumulate definitions and so on, but okay.  I guess
  // that kind of thing will come along as we go.

  val editor = new SimpleCardinalEditor[SimpleMarker]()

}
