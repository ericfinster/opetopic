/**
  * MutableComplex.scala - Cell Mutability Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Traverse
import scalaz.syntax.traverse._
import scalaz.std.option._

trait MutableComplex[A] { 

  type CellType <: MutableCell

  def cellFactory: CellFactory[A, CellType]

  trait MutableCell extends Cell[A, CellType] { thisCell: CellType => 

    def address: Option[SAddr]

    def findInParent: Option[SZipper[CellType]] = 
      for {
        parent <- container
        cn <- parent.canopy
        addr <- address
        pAddr <- addr.headOption.map(_.dir)
        z <- cn.seekTo(pAddr)
      } yield z

    def sourceDeriv[B]: SDeriv[B] = 
      sourceTree match {
        case None => SDeriv(SNode(SLeaf, SLeaf), SCtxt(Nil))  // Correct?
        case Some(srcs) => SDeriv(srcs.asShell)
      }

    // Is there a kind of "refresh canopy" routine?
    // The idea goes as follows: you look at the source tree.
    // now, you traverse it, and look at all the "outgoing"
    // fields.  If they are descendents of the current guy,
    // you keep going.  Something like this.  Is it possible?

    def extrudeAt(tgt: A, fill: A)(addr: SAddr, isSelected: CellType => Boolean): Option[Unit] = {

      for {
        currentFiller <- incoming
        _ = println("Got current filler")
        fzip <- currentFiller.findInParent
        _ = println("Found filler in parent")
        currentFillerContainer <- currentFiller.container
        _ = println("Incoming has a container")
        cn <- canopy
        _ = println("We have a canopy")
        z <- cn.seekTo(addr)
        _ = println("Seeking succeeded")
        pr <- z.focus.takeWhile(sourceDeriv, isSelected)
        _ = println("Cropped selection")
        (extracted, shell) = pr
        interiorRoot <- extracted.rootValue
        _ = println("Extracted tree is non-trivial")
      } yield {

        val targetCell = cellFactory.newCell(tgt, dim)
        targetCell.canopy = Some(extracted)
        targetCell.container = Some(this)
        targetCell.target = interiorRoot.target

        val fillerCell = cellFactory.newCell(fill, dim + 1)
        fillerCell.container = Some(currentFillerContainer)
        fillerCell.target = Some(targetCell)
        fillerCell.sourceTree = Some(extracted)

        //Is there a better way to do this?
        if (dim > 0) {
          targetCell.sourceTree = shell.traverse(_.rootValue)
        }

        extracted.foreach(c => {
          c.container = Some(targetCell)
          c.outgoing = Some(fillerCell)
        })

        val newCanopy = z.ctxt.close(SNode(targetCell, shell))
        canopy = Some(newCanopy)

        // Need to update the source tree of the current filler
        currentFiller.sourceTree = Some(newCanopy)

        // Okay, fzip is pointing at the current filler.
        // What happens next?  We could try to visit the 
        // direction we want, but I don't think that's it.

        fzip.focus match {
          case SLeaf => println("Focus is at a leaf")
          case SNode(fc, sh) => {

            // Okay, we should be looking at the outgoing shell
            // in the canopy, and we want to update this guy
            // accordingly.

            println("Focus has cell: " + fc.toString)


          }
        }

      }

    }

  }

}
