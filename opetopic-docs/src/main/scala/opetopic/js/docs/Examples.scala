/**
  * Examples.scala - Hold Example Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.docs

import opetopic._
import STree.obj

object Examples {

  val obj: SComplex[String] = ||(SDot("x"))
  val arrow: SComplex[String] = ||(SBox("y",SNode(SDot("x"),SLeaf))) >> SDot("f")
  val drop: SComplex[String] = ||(SDot("x")) >> SBox("f",SLeaf) >> SDot("α")
  val twoglob: SComplex[String] = ||(SBox("y",SNode(SDot("x"),SLeaf))) >> SBox("g",SNode(SDot("f"),SNode(SLeaf,SLeaf))) >> SDot("α")

  val simplex: SComplex[String] = 
    ||(SBox("z",SNode(SBox("y",SNode(SDot("x"),SLeaf)),SLeaf))) >>
      SBox("h",SNode(SDot("g"),SNode(SNode(SDot("f"),SNode(SLeaf,SLeaf)),SLeaf))) >> SDot("α")

  val quad: SComplex[String] = 
    ||(SBox("w",SNode(SBox("z",SNode(SBox("y",SNode(SDot("x"),SLeaf)),SLeaf)),SLeaf))) >> 
      SBox("k",SNode(SDot("h"),SNode(SNode(SDot("g"),SNode(SNode(SDot("f"),SNode(SLeaf,SLeaf)),SLeaf)),SLeaf))) >> 
      SDot("α")

  val threecell: SComplex[String] = 
    ||(SBox("e",SNode(SBox("d",SNode(SBox("c",SNode(SBox("b",SNode(SDot("a"),SLeaf)),SLeaf)),SLeaf)),SLeaf))) >> 
      SBox("n",SNode(SBox("m",SNode(SDot("i"),SNode(SNode(SBox("l",SLeaf),SNode(SNode(SBox("k",
        SNode(SDot("h"),SNode(SNode(SDot("g"),SNode(SLeaf,SLeaf)),SLeaf))),SNode(SNode(SBox("j",
          SNode(SDot("f"),SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf)),SLeaf)),SLeaf)),SLeaf))),SNode(SLeaf,SLeaf))) >>
      SBox("ζ",SNode(SDot("ε"),SNode(SNode(SDot("δ"),SNode(SLeaf,SNode(SNode(SNode(SDot("γ"),SLeaf),
        SNode(SNode(SNode(SDot("β"),SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf))),SNode(SNode(SNode(SDot("α"),
          SNode(SLeaf,SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf)),SLeaf)),SLeaf)),SLeaf))),SNode(SLeaf,SLeaf)))) >>
  SDot("Φ")

  val big: SComplex[String] = 
    ||(SBox("w",SNode(SBox("z",SNode(SBox("y",SNode(SDot("x"),SLeaf)),SLeaf)),SLeaf))) >>
      SBox("r",SNode(SBox("q",SNode(SDot("o"),SNode(SNode(SDot("n"),SNode(SLeaf,SLeaf)),SLeaf))),
        SNode(SNode(SBox("p",SLeaf),SNode(SNode(SDot("m"),SNode(SLeaf,SLeaf)),SLeaf)),SLeaf))) >>
      SBox("f",SNode(SBox("e",SNode(SDot("c"),SNode(SLeaf,SNode(SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf)),SLeaf)))),
        SNode(SNode(SDot("b"),SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf))),SNode(SNode(SNode(SBox("d",SNode(SDot("a"),SLeaf)),SLeaf),
          SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf)),SLeaf)))) >>
      SBox("ζ",SNode(SBox("ε",SNode(SDot("γ"),SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf))),
        SNode(SNode(SNode(SLeaf,SLeaf),SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf)),SLeaf))))),
        SNode(SNode(SBox("δ",SNode(SDot("β"),SNode(SLeaf,SNode(SLeaf,SNode(SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf)),SLeaf))))),
          SNode(SLeaf,SNode(SLeaf,SNode(SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf)),SLeaf)))),
          SNode(SNode(SLeaf,SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf))),SNode(SNode(SNode(SNode(SDot("α"),SNode(SLeaf,SLeaf)),SLeaf),
            SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf)),SLeaf))))) >>
      SBox("Θ",SNode(SDot("Ε"),SNode(SNode(SDot("Δ"),SNode(SLeaf,SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf))),
        SNode(SNode(SNode(SLeaf,SLeaf),SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf)),SLeaf))))),SNode(SNode(SNode(SDot("Γ"),
          SNode(SLeaf,SNode(SLeaf,SNode(SLeaf,SNode(SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf)),SLeaf))))),
          SNode(SLeaf,SNode(SLeaf,SNode(SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf)),SLeaf)))),
          SNode(SNode(SLeaf,SNode(SLeaf,SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf))),SNode(SNode(SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf),
            SNode(SNode(SLeaf,SNode(SLeaf,SLeaf)),SLeaf)),SLeaf)))))) >>
      SDot("Ω")

}
