/**
  * Cell.scala - Mutable opetopic cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mutable

import scalaz.Traverse
import scalaz.Applicative
import scalaz.syntax.traverse._
import scalaz.std.option._

trait Cell[A, C <: Cell[A, C]] { thisCell : C => 

  type CellTree = STree[C]

  var dim: Int
  var label: Option[A]

  //
  // Cell attributes
  //

  var canopy: Option[CellTree]
  var container: Option[C]

  var target : Option[C]
  var sourceTree: Option[CellTree]

  // 
  // Edge attributes
  //

  var incoming: Option[C]
  var outgoing: Option[C]

  //============================================================================================
  // HELPERS
  //

  def isBase: Boolean = 
    container == None

  def isDrop: Boolean = 
    sourceTree == None

  def isExternal: Boolean = 
    canopy == None

  def spine: Option[CellTree] = 
    canopy match {
      case None => {
        if (dim == 0) {
          // Handle object case by hand
          Some(SNode(thisCell, SNode(SLeaf, SLeaf)))
        } else {
          for {
            st <- sourceTree
          } yield SNode(thisCell, st.map(_ => SLeaf))
        }
      }
      case Some(cn) => {
        for {
          jn <- cn.traverse(_.spine)
          res <- STree.join(jn)
        } yield res
      }
    }


}


