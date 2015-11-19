/**
  * OpetopicPPrint.scala - OpetopicTT Implementation of PPrint
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

object OpetopicPPrint extends PPrintBase {

  //============================================================================================
  // IMPLEMENTATIONS
  //

  @natElim
  def pprintTree[A, N <: Nat](n: N)(tr: Tree[A, N], c: Config)(implicit ap: PPrint[A]): Iter[String] = {
    case (Z, Pt(a), c) => handleChunks(c, (c0: Config) => Iter(Iter("pt"), ap.render(a, c0)))
    case (S(p: P), Leaf(d), c) => handleChunks(c, (c0: Config) => Iter(Iter("leaf")))
    case (S(p: P), Node(a, sh), c) => 
      handleChunks(c, (c0: Config) => {

        object TreePP extends PPrint[Tree[A, S[P]]] {
          def render(t : Tree[A, S[P]], c1: Config) = 
            pprintTree(S(p))(t, c1)
        }

        Iter(Iter("node"), ap.render(a, c0), pprintTree(p)(sh, c0)(TreePP))
      })
  }

  def pprintNesting[A, N <: Nat](n: N)(nst: Nesting[A, N], c: Config)(implicit ap: PPrint[A]): Iter[String] = 
    nst match {
      case Obj(a) => handleChunks(c, (c0: Config) => Iter(Iter("obj"), ap.render(a, c0)))
      case Dot(a, d) => handleChunks(c, (c0: Config) => Iter(Iter("dot"), ap.render(a, c0)))
      case Box(a, cn) => 
        handleChunks(c, (c0: Config) => {

          object NestingPP extends PPrint[Nesting[A, N]] {
            def render(nst1: Nesting[A, N], c1: Config) =
              pprintNesting(n)(nst1, c1)
          }

          Iter(Iter("box"), ap.render(a, c0), pprintTree(n)(cn, c0)(NestingPP))
        })
    }

  //============================================================================================
  // CHUNKING IMPLEMENTATION
  //

  type ChunkFunc = Config => Iter[Iter[String]]
  
  def handleChunks(c: Config, chunkFunc: ChunkFunc): Iter[String] = {

    // Okay, how is this going to work.  You can't use the same method.
    // Basically, you want to iterate over each of the iterators.  The 
    // line break could occur somewhere in the middle.  What is supposed
    // to happen when this occurs?

    // Well ....

    val horizontalChunks = 
      chunkFunc(c).flatMap((i: Iter[String]) => {
        val str = i.toStream
        if (str.tail == Stream.Empty) 
          " " +: str
        else
          (" (" +: str) :+ ")"
      }).toStream.drop(1)

    val effectiveWidth = c.width - (c.depth * c.indent)

    @tailrec def checkOverflow(chunks: Stream[String], currentWidth: Int): Boolean = chunks match{
      case Stream.Empty => false
      case head #:: rest =>
        if (head.contains("\n")) true
        else {
          val nextWidth = currentWidth + head.length
          if (nextWidth > effectiveWidth) true
          else checkOverflow(rest, nextWidth)
        }
    }

    val overflow = checkOverflow(horizontalChunks, 0)

    if (overflow) 
      handleVerticalChunks(c, chunkFunc)
    else 
      horizontalChunks.iterator

    // idea, something like:
    // handleChunks(c.deeper, (c: Config) => chunkFunc(c).drop(id))

  }

  def handleVerticalChunks(c: Config, chunkFunc: ChunkFunc): Iter[String] = {

    val chunks2 = chunkFunc(c.deeper)

    // Needs to be a def to avoid exhaustion
    def indent = Iter.fill(c.depth)(" ")

    chunks2.flatMap(Iter("\n") ++ indent ++ _).drop(1)

  }

}
