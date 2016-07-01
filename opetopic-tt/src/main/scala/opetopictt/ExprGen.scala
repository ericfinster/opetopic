/**
  * ExprGen.scala - Generate type theory terms from opetopes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopictt

import opetopic._
import mtl._

import opetopictt.pprint._
import Token._
import Tokenizer._

object ExprGen {

  sealed trait Term
  case class Var(id: String) extends Term
  case class Dot(i: Term) extends Term
  case class Box(t: Term, gm: Map[SAddr, Term]) extends Term

  implicit object TermTokenizer extends Tokenizer[Term] {

    def addrStr(addr: SAddr): String = 
      addr.map(d => addrStr(d.dir)).mkString("{", "|", "}")

    def mapToPattern(m: Map[SAddr, Term]) : Token = {
      val tks : List[Token] = m.map({ 
        case (addr, t) => Phrase(Literal(addrStr(addr)), Literal("=>"), t.parenthesize, Literal("|"))
      }).toList

      Phrase(Delim("[", Phrase(tks : _*), "]"))
    }

    def tokenize(t: Term) : Token = 
      t match {
        case Var(id) => Literal(id)
        case Dot(t) => Phrase(Literal("dot"), t.parenthesize)
        case Box(t, gm) => Phrase(Literal("box"), t.parenthesize, mapToPattern(gm))
        case _ => Literal("unknown")
      }

  }

  def exprGen(c: SComplex[String]): Option[Term] = 
    c match {
      case ||(SDot(i)) => Some(Var(i))  // Object case
      case ||(SBox(i, SNode(SDot(p), SLeaf))) >> SDot(c) => Some(Dot(Var(p)))
      case tl >> (hd @ SBox(_, cn)) >> SDot(a) => {

        val frm : SComplex[String] = tl >> hd

        def verticalPass(tr : STree[SNesting[String]], addr: SAddr): Option[(SAddr, Term)] = 
          tr match {
            case SLeaf => 
              for {
                thisLeaf <- tl.sourceAt(addr)
                leafTerm <- exprGen(thisLeaf)
              } yield (addr, Dot(leafTerm))
            case SNode(n, sh) => 
              for {
                thisFace <- frm.sourceAt(SDir(addr) :: Nil)
                thisTerm <- exprGen(thisFace)
                trMap <- sh.traverseWithAddr((b, dir) => {
                  verticalPass(b, SDir(dir) :: addr)
                })
              } yield (addr, Box(thisTerm, Map(trMap.toList : _*)))
          }

        verticalPass(cn, Nil).map(_._2)

      }
      case _ => None  // Everything else is bad
    }

// box (box (dot a) [{{}} => (dot a) |]) 
//   [
//     {{}} => (box (box (dot d) 
//                  [
//                    {{}} => (box (dot d) [{{}|{}} => (box (dot b) [{{}|{}|{}} => (box (dot a) [{{}|{}|{}|{}} => (dot a) |]) |]) |]) 
//                  |])
//       [
//         {{}|{}} => (dot (dot d)) | 
//         {{{}}|{}} => (box (dot d) []) | 
//         {{{}|{}}|{}} => (box (box (dot c) [{{}} => (box (dot b) [{{}|{}} => (dot b) |]) |]) [{{}|{{}|{}}|{}} => (dot (dot c)) | {{{}}|{{}|{}}|{}} => (dot (dot b)) |]) | 
//         {{{}|{}|{}}|{}} => (box (box (dot a) [{{}} => (dot a) |]) [{{}|{{}|{}|{}}|{}} => (dot (dot a)) |]) 
//       |]) 
//   |]

}

object Examples {

  val obj: SComplex[String] = ||(SDot("i"))
  val arrow: SComplex[String] = ||(SBox("i",SNode(SDot("p"),SLeaf))) >> SDot("c")
  val drop: SComplex[String] = ||(SDot("i")) >> SBox("c",SLeaf) >> SDot("α")
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

}


