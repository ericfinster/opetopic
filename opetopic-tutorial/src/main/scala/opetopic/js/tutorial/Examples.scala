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

  val threecell : Complex[OptStr, _3] = 
    Complex[OptStr] >> Box( Some("e"), Pt(Box(Some("d"), Pt(Box(Some("c"), Pt(Box(Some("b"), Pt(Obj(Some("a"))))))))) ) >> Box( Some("n"), Node( Box( Some("m"), Node( Dot(Some("i"), S(Z)), Pt( Node( Box(Some("l"), Leaf(S(Z))), Pt( Node( Box( Some("k"), Node( Dot(Some("h"), S(Z)), Pt(Node(Dot(Some("g"), S(Z)), Pt(Leaf(S(Z))))) ) ), Pt( Node( Box(Some("j"), Node(Dot(Some("f"), S(Z)), Pt(Leaf(S(Z))))), Pt(Leaf(S(Z))) ) ) ) ) ) ) ) ), Pt(Leaf(S(Z))) ) ) >> Box( Some("\u03b6"), Node( Dot(Some("\u03b5"), S(S(Z))), Node( Node( Dot(Some("\u03b4"), S(S(Z))), Node( Leaf(S(S(Z))), Pt( Node( Node(Dot(Some("\u03b3"), S(S(Z))), Leaf(S(Z))), Pt( Node( Node( Dot(Some("\u03b2"), S(S(Z))), Node(Leaf(S(S(Z))), Pt(Node(Leaf(S(S(Z))), Pt(Leaf(S(Z)))))) ), Pt( Node( Node( Dot(Some("\u03b1"), S(S(Z))), Node(Leaf(S(S(Z))), Pt(Leaf(S(Z)))) ), Pt(Leaf(S(Z))) ) ) ) ) ) ) ) ), Pt(Leaf(S(Z))) ) ) ) >> Dot(Some("\u03a6"), S(S(S(Z))))

}
