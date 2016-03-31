/**
  * Pickle.scala - Custom Pickler implementations for Opetopic types
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import upickle.Js
import upickle.default._

object Pickler {

  def treeWriter[A, N <: Nat] : Writer[Tree[A, N]] = ???

  class CustomThing2(val i: Int, val s: String)
  object CustomThing2{
    implicit val thing2Writer = Writer[CustomThing2]{
      case t => Js.Str(t.i + " " + t.s)
    }
    implicit val thing2Reader = Reader[CustomThing2]{
      case Js.Str(str) =>
        val Array(i, s) = str.split(" ")
        new CustomThing2(i.toInt, s)
    }
  }

  def test = println("Here's a test")

}
