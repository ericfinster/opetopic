/**
  * Cell.scala - Mutable opetopic cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Traverse
import scalaz.Applicative
import scalaz.syntax.traverse._
import scalaz.std.option._

trait Cell[A, C <: Cell[A, C]] { thisCell : C => 

  var dim: Int = 0
  var label: Option[A]

  //
  // Cell attributes
  //

  var canopy: Option[STree[C]] = None
  var container: Option[C] = None

  var target : Option[C] = None
  var sourceTree: Option[STree[C]] = None

  // 
  // Edge attributes
  //

  var incoming: Option[C] = None
  var outgoing: Option[C] = None

  //============================================================================================
  // HELPERS
  //

  def isBase: Boolean = 
    container == None

  def isDrop: Boolean = 
    sourceTree == None

  def isExternal: Boolean = 
    canopy == None

  def spine: Option[STree[C]] = 
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

  def interiorCells: List[C] = 
    canopy match {
      case None => List(thisCell)
      case Some(cn) => thisCell :: cn.toList.map(_.interiorCells).flatten
    }

}


