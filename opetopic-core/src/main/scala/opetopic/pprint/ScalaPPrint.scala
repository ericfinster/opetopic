/**
  * ScalaPPrint.scala - PPrint Implementation for Scala
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.pprint

import scala.{Iterator => Iter}
import annotation.tailrec

import opetopic._
import syntax.tree._
import syntax.nesting._
import syntax.complex._

object ScalaPPrint extends PPrintBase {

  //============================================================================================
  // IMPLEMENTATIONS
  //

  @natElim
  def pprintTree[A, N <: Nat](n: N)(tr: Tree[A, N], c: Config)(implicit ap: PPrint[A]): Iter[String] = {
    case (Z, Pt(a), c) => handleChunks("Pt", c, (c0: Config) => Iter(ap.render(a, c0)))
    case (S(p: P), Leaf(d), c) => handleChunks("Leaf", c, (c0: Config) => Iter(pprintNat(d, c0)))
    case (S(p: P), Node(a, sh), c) => 
      handleChunks("Node", c, (c0: Config) => {

        object TreePP extends PPrint[Tree[A, S[P]]] {
          def render(t : Tree[A, S[P]], c1: Config) = 
            pprintTree(S(p))(t, c1)
        }

        Iter(ap.render(a, c0), pprintTree(p)(sh, c0)(TreePP))
      })
  }

  def pprintNesting[A, N <: Nat](n: N)(nst: Nesting[A, N], c: Config)(implicit ap: PPrint[A]): Iter[String] = 
    nst match {
      case Obj(a) => handleChunks("Obj", c, (c0: Config) => Iter(ap.render(a, c0)))
      case Dot(a, d) => handleChunks("Dot", c, (c0: Config) => Iter(ap.render(a, c0), pprintNat(d, c0)))
      case Box(a, cn) => 
        handleChunks("Box", c, (c0: Config) => {

          object NestingPP extends PPrint[Nesting[A, N]] {
            def render(nst1: Nesting[A, N], c1: Config) =
              pprintNesting(n)(nst1, c1)
          }

          Iter(ap.render(a, c0), pprintTree(n)(cn, c0)(NestingPP))
        })
    }

  @natElim
  def doPprintComplex[A[_ <: Nat], N <: Nat](n: N)(cmplx: Complex[A, N], c: Config, ap: IndexedPPrint[A]) : Iter[String] = {
    case (Z, Complex(tl, hd), c, ap) => {
      implicit val npp = atIndex(Z, ap)
      pprintNesting(Z)(hd, c)
    }
    case (S(p), Complex(tl, hd), c, ap) => {
      implicit val npp = atIndex(S(p), ap)
      doPprintComplex(p)(tl, c, ap) ++ Iter(" >> ") ++ pprintNesting(S(p))(hd, c)
    }
  }

  def pprintComplex[A[_ <: Nat], N <: Nat](cmplx: Complex[A, N])(implicit c: Config, ap: IndexedPPrint[A]) : Iter[String] = 
    doPprintComplex(cmplx.dim)(cmplx, c, ap)

  def pprintNat[N <: Nat](n: N, c: Config) : Iter[String] = 
    n match {
      case Z => Iter("Z")
      case S(p) => handleChunks("S", c, (c0: Config) => Iter(pprintNat(p, c0)))
    }

  //============================================================================================
  // CHUNKING IMPLEMENTATION
  //

  type ChunkFunc = Config => Iter[Iter[String]]
  
  val ansiRegex = "\u001B\\[[;\\d]*m"

  def handleChunks(name: String, c: Config, chunkFunc: ChunkFunc): Iter[String] = {

    // Prefix, contents, and all the extra ", " "(" ")" characters
    val horizontalChunks =
      chunkFunc(c).flatMap(", " +: _.toStream)
                  .toStream
                  .drop(1)

    val effectiveWidth = c.width - (c.depth * c.indent)

    @tailrec def checkOverflow(chunks: Stream[String], currentWidth: Int): Boolean = chunks match{
      case Stream.Empty => false
      case head #:: rest =>
        if (head.contains("\n")) true
        else {
          val nextWidth = currentWidth + head.replaceAll(ansiRegex, "").length
          if (nextWidth > effectiveWidth) true
          else checkOverflow(rest, nextWidth)
        }
    }

    val overflow = checkOverflow(horizontalChunks, name.length + 2)

    if (overflow) 
      handleVerticalChunks(name, c, chunkFunc)
    else 
      Iter(name, "(") ++ horizontalChunks ++ Iter(")")

  }

  def handleVerticalChunks(name: String, c: Config, chunkFunc: ChunkFunc): Iter[String] = {

    val chunks2 = chunkFunc(c.deeper)

    // Needs to be a def to avoid exhaustion
    def indent = Iter.fill(c.depth)("  ")

    Iter(name, "(\n") ++ 
      chunks2.flatMap(Iter(",\n", "  ") ++ indent ++ _).drop(1) ++ 
      Iter("\n") ++ indent ++ Iter(")")

  }

}

