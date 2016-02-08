/**
  * Examples.scala - Hold Example Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.tutorial

import opetopic._

object Examples {

  import opetopic._

  type OptStr[N <: Nat] = Option[String]

  val arrow : Complex[OptStr, _1] = Complex[OptStr] >> Box(Some("y"), Pt(Obj(Some("x")))) >> Dot(Some("f"), S(Z))

  val simplex : Complex[OptStr, _2] = 
    Complex[OptStr] >> Box(Some("z"), Pt(Box(Some("y"), Pt(Obj(Some("x")))))) >> 
      Box(Some("h"), Node(Dot(Some("g"), S(Z)), Pt(Node(Dot(Some("f"), S(Z)), Pt(Leaf(S(Z)))))) ) >> Dot(Some("\u03b1"), S(S(Z)))

}
