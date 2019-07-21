/**
  * PPrint.scala - Some basic pretty printing routines
  *
  * Algorithm adapted from:
  *  http://www.lihaoyi.com/post/CompactStreamingPrettyPrintingofHierarchicalData.html
  *  
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.syntax

import collection.mutable.Buffer

object PrettyPrinter {

  //
  //  Hierarchical printable data (rename this?)
  //

  sealed trait Tree

  case class Nested(
    prefix: String,
    children: Iterator[Tree],
    sep: String,
    suffix: String) extends Tree

  case class Literal(body: String) extends Tree

  //
  // Main entry point for printing algo
  //

  def prettyPrint(t: Tree, maxWidth: Int, indent: Int): Iterator[String] = {

    // The recursive step....
    def recurse(current: Tree, leftOffset: Int, enclosingSepWidth: Int): (Boolean, Iterator[String]) = {
      current match{

        // For a literal, just look for line separators and return a singleton iterator.
        case Literal(body) => {
          val multiLine = body.contains('\n')
          val chunks = Iterator(body)
          (multiLine, chunks)
        }

        case Nested(prefix, children, sep, suffix) =>
          var usedWidth = leftOffset + prefix.length + suffix.length + enclosingSepWidth
          var multiLine = usedWidth > maxWidth
          val allChildChunks = Buffer[Iterator[String]]()

          // Prepare all child iterators, but do not actually consume them

          for(child <- children){
            val (childMultiLine, childChunks) = recurse(
              child,
              leftOffset + indent,
              if (children.hasNext) sep.trim.length else 0
            )
            if (childMultiLine) multiLine = true
            allChildChunks += childChunks
          }
          val bufferedChunks = Buffer[Buffer[String]]()

          val outChunkIterator = allChildChunks.iterator

          var remainingIterator: Iterator[String] = Iterator.empty

          // Buffer child node chunks, until they run out or we become multiline
          while(outChunkIterator.hasNext && !multiLine){
            bufferedChunks.append(Buffer())
            val childIterator = outChunkIterator.next()

            if (outChunkIterator.hasNext) usedWidth += sep.length

            while (childIterator.hasNext && !multiLine){
              val chunk = childIterator.next()
              bufferedChunks.last.append(chunk)
              usedWidth += chunk.length
              if (usedWidth > maxWidth) {
                remainingIterator = childIterator
                multiLine = true
              }
            }
          }

          def joinIterators(separated: Iterator[TraversableOnce[String]],
            sepChunks: Seq[String]) = {
            separated.flatMap(sepChunks ++ _).drop(1)
          }

          val middleChunks =
            if (!multiLine) {
              // If not multiline, just join all child chunks by the separator
              joinIterators(bufferedChunks.iterator, Seq(sep))
            } else{
              // If multiline, piece back together the last half-consumed iterator
              // of the last child we half-buffered before we stopped buffering.
              val middleChildChunks = bufferedChunks.lastOption.map(_.iterator ++ remainingIterator)

              // Splice it in between the chunks of the fully-buffered children
              // and the not-at-all buffered children, joined with separators
              joinIterators(
                separated = bufferedChunks.dropRight(1).iterator ++
                  middleChildChunks ++
                  outChunkIterator,
                sepChunks = Seq(sep.trim, "\n", " " * (leftOffset + indent))
              ) ++ Iterator("\n", " " * leftOffset)
            }

          val chunks = Iterator(prefix) ++ middleChunks ++ Iterator(suffix)
          (multiLine, chunks)
      }
    }

    val (_, chunks) = recurse(t, 0, 0)
    chunks

  }


}

//
//  Here is the structure I was using previously.  Similar idea ...
//  perhaps it could be of use at some point.
//

// object PPrint {

//   sealed trait Token
//   case class Phrase(seq: Token*) extends Token
//   case class Literal(str: String) extends Token
//   case class Delim(ld: String, t: Token, rd: String) extends Token
  
//   def printToken(t : Token) : String =
//     t match {
//       case Literal(str) => str
//       case Phrase(seq @ _*) => seq.map(printToken(_)).mkString(" ")
//       case Delim(ld, t, rd) => ld + printToken(t) + rd
//     }

//   def pprint[A](a: A)(implicit tt: Tokenizable[A]): String =
//     printToken(tt.tokenize(a))

//   trait Tokenizable[A] {
//     def tokenize(a: A): Token
//   }

//   implicit class TokenizableOps[A](a: A)(implicit tt: Tokenizable[A]) {

//     def parenthesize: Token =
//       tt.tokenize(a) match {
//         case p@Phrase(seq @ _*) => if (seq.length > 1) Delim("(", p, ")") else p
//         case t => t
//       }

//     def tokenize: Token =
//       tt.tokenize(a)
    
//   }

//   implicit val isTokenizableString : Tokenizable[String] =
//     new Tokenizable[String] {
//       def tokenize(s: String) = Literal(s)
//     }

// }
