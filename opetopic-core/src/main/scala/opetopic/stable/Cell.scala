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

  def spine(guide: STree[C]): Option[STree[C]] = 
    (canopy, guide) match {
      case (Some(cn), SNode(_, sh)) => 
        for {
          toJn <- cn.matchTraverse(sh)({
            case (desc, vbr) => desc.spine(vbr)
          })
          res <- STree.join(toJn)
        } yield res
      case (None, SNode(_, _)) => None // Guide error
      case (_, _) => 
        for {
          srcs <- sourceTree
        } yield SNode(thisCell, srcs.map(_ => SLeaf))
    }

  // Duplicate up to the given guide, remembering the canopy which
  // was truncated off.
  def duplicateWithGuide(cf: CellFactory[A, C], guide: STree[C]) : Option[(C, STree[STree[C]])] = 
    (canopy, guide) match {
      case (Some(cn), SLeaf) => 
        for {
          srcs <- sourceTree
        } yield (cf.newCell(label), SNode(cn, srcs.map(_ => SLeaf)))
      case (Some(cn), SNode(_, sh)) => {
        val nc = cf.newCell(label)
        for {
          prTr <- cn.matchTraverse(sh)({
            case (d, v) => d.duplicateWithGuide(cf, v)
          })
          pr = STree.unzip(prTr)
          (ncn, toJn) = pr
          rc <- STree.join(toJn)
          _ = ncn.map(_.container = Some(nc))
        } yield (nc, rc)
      }
      case (None, SLeaf) => 
        for {
          srcs <- sourceTree
          cst = srcs.map(_ => SLeaf)
        } yield (cf.newCell(label), SNode(SNode(thisCell, cst), cst))
      case (None, SNode(_, _)) => None // An error in the guide
    }

  def compressWithGuide(guide: STree[STree[C]]) : Option[C] = 
    guide match {
      case SNode(sk, sh) => 
        for {
          sp <- spine(sk)
          ds <- sp.matchTraverse(sh)({
            case (d, v) => d.compressWithGuide(v)
          })
          _ = ds.map(d => d.container = Some(thisCell))
          _ = thisCell.canopy = Some(ds)
        } yield thisCell
      case SLeaf => Some(thisCell)
    }

  def extractWithGuide(cf: CellFactory[A, C], guide: STree[C]) : Option[C] =
    for {
      tgt <- target
      sp <- spine
      prevBase <- tgt.extractWithGuide(cf, sp)
      pr <- duplicateWithGuide(cf, guide)
      (thisBase, toCompress) = pr
      _ <- prevBase.compressWithGuide(toCompress)
    } yield thisBase

  def face(cf: CellFactory[A, C]): Option[C] = {
    val nc = cf.newCell(label)
    for {
      srcs <- sourceTree
      sp <- spine
      tmp <- extractWithGuide(cf, sp)
      nt <- compressWithGuide(SNode(sp, srcs.map(_ => SLeaf)))
    } yield nc
  }


}


trait CellFactory[A, C <: Cell[A, C]] {

  def newCell(opt: Option[A]) : C

}
