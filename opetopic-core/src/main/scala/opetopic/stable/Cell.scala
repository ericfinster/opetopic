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
  def label: A

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

  def parent: Option[C] = 
    for {
      tgt <- target
      p <- tgt.outgoing
    } yield p

  def interiorCells: List[C] = 
    canopy match {
      case None => List(thisCell)
      case Some(cn) => thisCell :: cn.toList.map(_.interiorCells).flatten
    }

  def verticalTree: STree[C] = 
    canopy match {
      case None => SLeaf
      case Some(cn) => SNode(thisCell, cn.map(_.verticalTree))
    }

  def targets: List[C] = 
    target match {
      case None => List(thisCell)
      case Some(tgt) => thisCell :: tgt.targets
    }

  def corollaWith[B](b: B) : STree[B] = 
    sourceTree match {
      case None => SNode(b, SNode(SLeaf, SLeaf))
      case Some(srcs) => SNode(b, srcs.map(_ => SLeaf))
    }

  def spine: Option[STree[C]] = 
    canopy match {
      case None => Some(corollaWith(thisCell))
      case Some(cn) => {
        for {
          jn <- cn.traverse(_.spine)
          res <- STree.join(jn)
        } yield res
      }
    }

  def spine[D <: Cell[A, D]](guide: STree[D]): Option[STree[C]] = 
    guide match {
      case SLeaf => Some(corollaWith(thisCell))
      case SNode(_, sh) =>
        for {
          cn <- canopy
          toJn <- cn.matchTraverse(sh)({
            case (d, v) => d.spine(v)
          })
          res <- STree.join(toJn)
        } yield res
    }

  def refreshWith(lvs: STree[C]) : Option[C] = 
    canopy match {
      case None => target
      case Some(cn) => {

        sourceTree = Some(lvs)

        for {
          myRoot <- cn.graftRec[C]({
            case addr => lvs.elementAt(addr)
          })({
            case (sc, tr) => sc.refreshWith(tr)
          })
          _ = target = Some(myRoot)
        } yield myRoot

      }
    }

  def duplicateWithGuide[D <: Cell[A, D]](cf: CellFactory[A, D], guide: STree[C]) : Option[(D, STree[(D, STree[C])])] = {
    (canopy, guide) match {
      case (_, SLeaf) => {
        val nc = cf.newCell(label, dim)
        for {
          sp <- spine
        } yield (nc, corollaWith((nc, sp)))
      }
      case (Some(cn), SNode(_, sh)) => {
        val nc = cf.newCell(label, dim)
        for {
          prTr <- cn.matchTraverse(sh)({
            case (d, v) => d.duplicateWithGuide(cf, v)
          })
          pr = STree.unzip(prTr)
          (ncn, toJn) = pr
          jnPr <- STree.join(toJn)
          _ = ncn.map(_.container = Some(nc))
          _ = nc.canopy = Some(ncn)
        } yield (nc, jnPr)
      }
      case (None, SNode(c, _)) => None // An error in the guide
    }
  }

  def compressWithGuide[D <: Cell[A, D]](cf: CellFactory[A, C], guide: STree[STree[D]]) : Option[C] = {
    guide match {
      case SNode(SLeaf, sh) => {
        // This is the case where we must duplicate ...
        val nc = cf.newCell(label, dim)
        nc.container = Some(thisCell)
        nc.sourceTree = sourceTree
        nc.target = target
        canopy = Some(corollaWith(nc))
        for {
          root <- sh.rootValue
          ds <- nc.compressWithGuide(cf, root)
        } yield thisCell
      }
      case SNode(sk, sh) => 
        for {
          sp <- spine(sk)
          ds <- sp.matchTraverse(sh)({
            case (d, v) => d.compressWithGuide(cf, v)
          })
          _ = ds.map(d => d.container = Some(thisCell))
          _ = thisCell.canopy = Some(ds)
        } yield thisCell
      case SLeaf => Some(thisCell)
    }
  }

  def extractWithGuide[D <: Cell[A, D]](cf: CellFactory[A, D], guide: STree[C]) : Option[D] =
    for {
      pr <- duplicateWithGuide(cf, guide)
      (thisBase, joinedShell) = pr
      (localSpine, toCompress) = STree.unzip(joinedShell)
      sp <- STree.join(toCompress)
      _ <- target match {
        case None => Some(())
        case Some(tgt) => 
          for {
            prevBase <- tgt.extractWithGuide(cf, sp)
            _ <- prevBase.compressWithGuide(cf, toCompress)
            _ <- localSpine.matchTraverse(prevBase.verticalTree)({
              case (sc, et) => Some({
                sc.sourceTree = et.canopy
                sc.target = Some(et)
                et.incoming = Some(sc)
                et.canopy.map(_.map(s => { s.outgoing = Some(sc) }))
              })
            })
            psp <- prevBase.spine
            _ <- thisBase.refreshWith(psp)
          } yield ()
      }
    } yield thisBase

  def face[D <: Cell[A, D]](cf: CellFactory[A, D]): Option[D] = {

    val nc = cf.newCell(label, dim)

    sourceTree match {
      case None => Some(nc) // Case of an object
      case Some(srcs) => 
        for {
          sp <- spine
          tgt <- target
          tmp <- tgt.extractWithGuide(cf, sp)
          nt <- tmp.compressWithGuide(cf, SNode(sp, srcs.map(_ => SLeaf)))
          _ = nt.incoming = Some(nc)
          _ = nt.canopy.map(_.map(s => { s.outgoing = Some(nc) }))
          _ = nc.target = Some(nt)
          _ = nc.sourceTree = nt.canopy
        } yield nc
    }

  }

}


trait CellFactory[A, C <: Cell[A, C]] {

  def newCell(a: A, dim: Int) : C

}

