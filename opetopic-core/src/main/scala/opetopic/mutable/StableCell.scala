/**
  * StableCell.scala - Mutable Cells based on stability
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mutable

import scalaz.Traverse
import scalaz.Applicative
import scalaz.syntax.traverse._
import scalaz.std.option._

class StableCell[A] {

  type CellTree = STree[StableCell[A]]

  var dim: Int = 0
  var label: Option[A] = None

  var canopy: Option[CellTree] = None
  var container: Option[SimpleCell[A]] = None

  var target: Option[SimpleCell[A]] = None
  var sourceTree: Option[CellTree] = None

  var incoming: Option[SimpleCell[A]] = None
  var outgoing: Option[SimpleCell[A]] = None

  //============================================================================================
  // FACES, ETC.
  //

  def partialSpine(guide: CellTree): Option[CellTree] = 
    (guide, canopy) match {
      case (SNode(c, cs), Some(cn)) => {

        val theMatch : Option[STree[(CellTree, StableCell[A])]] = 
          cs.matchWith(cn)

        // This is the interesting case.

        ???

      }
      case _ => {

        // In any other case, we have run out of room.  Hence
        // we simply look at the source tree and fill it with 
        // leaves to represent the corolla at this point.

        for {
          st <- sourceTree
        } yield SNode(this, st.map(_ => SLeaf))

      }

    }
}
