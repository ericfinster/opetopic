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
  var container: Option[StableCell[A]] = None

  var target: Option[StableCell[A]] = None
  var sourceTree: Option[CellTree] = None

  var incoming: Option[StableCell[A]] = None
  var outgoing: Option[StableCell[A]] = None

  //============================================================================================
  // TRAVERSALS
  //

  def foreachInterior(f: StableCell[A] => Unit): Unit = 
    canopy match {
      case None => f(this)
      case Some(cn) => {
        for {
          c <- cn
        } yield c.foreachInterior(f)
        f(this)
      }
    }

  //============================================================================================
  // FACES, ETC.
  //


  // So it seems if we calculate the spine, and then extract the partial, 
  // that that should in fact be the source tree, no?

  def interiorRoot: Option[StableCell[A]] = 
    canopy match {
      case None => Some(this)
      case Some(SLeaf) => None 
      case Some(SNode(r, _)) => r.interiorRoot
    }

  def getTarget: Option[StableCell[A]] = 
    for {
      ir <- interiorRoot
      tgt <- ir.target
    } yield tgt

  def getSourceTree: Option[CellTree] = 
    for {
      sp <- spine
      tgt <- target
      st <- tgt.partialSpine(sp)
    } yield st

  def spine: Option[CellTree] = 
    canopy match {
      case None => 
        for {
          st <- sourceTree
        } yield SNode(this, st.map(_ => SLeaf))
      case Some(cn) => 
        for {
          jn <- cn.traverse(_.spine)
          res <- STree.join(jn)
        } yield res
    }

  def partialSpine(guide: CellTree): Option[CellTree] = 
    (guide, canopy) match {
      case (SNode(c, cs), Some(cn)) => {

        // In this case, we expect the shell of our tree to 
        // agree with the canopy of the cell.  So we match
        // them and continue the traverse.

        // As we descend back down, we join all of the resulting
        // cells and so on.

        for {
          jn <- cs.matchTraverse(cn)({
            case (br, sc) => sc.partialSpine(br)
          })
          r <- STree.join(jn)
        } yield r

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

  // The idea of the face algorithm should not be too far from here
  // now.  Essentially, you iteratively calculate the partial spine
  // of all the targets.  As one now iterates back up the structure,
  // you will traverse the partial spines, copying out the source and
  // targets from any internal nodes and adjoining them to the results
  // from lower dimensions.

  // It's quite interesting, and it should extract exactly a face, or
  // I suppose more generally, a pasting diagram of such faces.

  def extractWith(guide: CellTree) : Option[StableCell[A]] = {

    guide match {
      case SLeaf => {

        // At a leaf, we should copy the current cell
        // and return it ....

        val thisCell = new StableCell[A]
        thisCell.label = label

        Some(thisCell)

      }
      case SNode(c, cs) => {

        // We are the target cell.  Copy ourselves, etc.
        val targetCell = new StableCell[A]
        targetCell.label = label

        // The external cell is a copy of c, the node in our guide
        val externalCell = new StableCell[A]
        externalCell.label = c.label

        for {
          cn <- canopy
          ncn <- cn.matchTraverse(cs)({
            case (desc, br) => 
              for {
                sc <- desc.extractWith(br)
              } yield {
                sc.container = Some(targetCell)
                sc.outgoing = Some(externalCell)
                sc
              }
          })
        } yield {

          externalCell.sourceTree = Some(ncn)
          externalCell.target = Some(targetCell)

          targetCell.canopy = Some(ncn)
          targetCell.incoming = Some(externalCell)
          targetCell

        }
      }
    }

  }

}

object StableCell {

  import opetopic._
  import syntax.tree._
  import syntax.nesting._
  import syntax.complex._

  //============================================================================================
  // BASIC CONTSTRUCTORS
  //

  def apply[A](opt: Option[A]): StableCell[A] = {
    val sc = new StableCell[A]
    sc.label = opt
    sc
  }

  def apply[A, N <: Nat](opt: Option[A], n: N): StableCell[A] = {
    val sc = new StableCell[A]
    sc.label = opt
    sc.dim = natToInt(n)
    sc
  }

  //============================================================================================
  // FROM NESTINGS AND COMPLEXES
  //

  def fromBox[A, N <: Nat](b: Box[Option[A], N]): Nesting[StableCell[A], N] = 
    b match {
      case Box(a, cn) => {

        val n = cn.dim
        val cell = StableCell(a, n)
        val newCn = cn.map(fromNesting(_))

        val canopy = STree(n)(newCn.map((nn: Nesting[StableCell[A], N]) => {
          nn.baseValue.container = Some(cell)
          nn.baseValue
        }))

        cell.canopy = Some(canopy)
        Box(cell, newCn)

      }
    }

  @natElim
  def fromNesting[A, N <: Nat](n: N)(nst: Nesting[Option[A], N]) : Nesting[StableCell[A], N] = {
    case (Z, Obj(o)) => Obj(StableCell(o))
    case (Z, b @ Box(a, cn)) => fromBox(b)
    case (S(p), Dot(o, _)) => Dot(StableCell(o, S(p)), S(p))
    case (S(p), b @ Box(a, cn)) => fromBox(b)
  }

  def fromNesting[A, N <: Nat](nst: Nesting[Option[A], N]) : Nesting[StableCell[A], N] = 
    fromNesting(nst.dim)(nst)

  def bond[A, P <: Nat](m: Nesting[StableCell[A], P], n: Nesting[StableCell[A], S[P]]) : ShapeM[Unit] = 
    (m, n) match {
      case (m, Box(cell, cn)) => 
        for {
          sp <- Nesting.spineFromCanopy(cn)
          _ <- Tree.matchTraverse(m.toTree, sp)({
            case (t, c) => {

              // Setup remaing bond information

              c.sourceTree = t.canopy
              c.target = Some(t)

              t.incoming = Some(c)
              t.canopy.map(_.map(s => { s.outgoing = Some(s) }))

              succeed(())

            }
          })
        } yield {

          // A hack to see what happens...
          cell.foreachInterior(c => {
            c.target = c.getTarget
          })

          // Now, what's left missing here is to set the source and targets
          // of all the remaining cells *under* the canopy.  That is, we
          // must propogate the changes down.  How should this be done?

          // Well, for example, you can test out the spine calculation, since
          // this would be one way to understand the sources, no?

          // Actually, for now I would think you care mostly just about the target.
          // Well, then convert the above nesting to a tree, and do a graft rec or
          // something to propogate the target ...

          // Ah, no, targets in the middle are more complicated.  So there's a non-trivial
          // problem here.

          // Exactly.  These sorts of operations stumble all the way down to the lowest levels,
          // and are intimately related to calculating faces, compressing and so on.

        }
      case (Box(t, cn), Dot(c, dm)) => {

        c.sourceTree = t.canopy
        c.target = Some(t)

        t.incoming = Some(c)
        t.canopy.map(_.map(s => { s.outgoing = Some(c) }))

        succeed(())

      }
      case (_, _) => fail("Unexpected bonding condition")
    }

  class ComplexBuilder[A] {

    type OptA[N <: Nat] = Option[A]

    @natElim
    def fromComplex[N <: Nat](n: N)(c: Complex[OptA, N]): ShapeM[Nesting[StableCell[A], N]] = {
      case (Z, Complex(_, objs)) => succeed(fromNesting(objs))
      case (S(p: P), Complex(tl, hd)) => 
        for {
          blorp <- fromComplex(p)(tl)
          bleep = fromNesting(hd)
          _ <- bond(blorp, bleep)
        } yield bleep
    }

    @natElim
    def toComplex[N <: Nat](n: N)(sc: StableCell[A]): Option[Complex[OptA, N]] = {
      case (Z, sc) => 
        for {
          objNst <- toNesting(Z)(sc)
          _ = println("Parse object nesting")
        } yield Complex[OptA] >> objNst
      case (S(p: P), sc) => {
        println("Making complex in dimension " + natToInt(S(p)).toString)
        for {
          tgt <- sc.target
          _ = println("Got target")
          tl <- toComplex(p)(tgt)
          hd <- toNesting(S(p))(sc)
          _ = println("Parsed nesting in dim " + natToInt(S(p)).toString)
        } yield tl >> hd
      }

    }

  }

  //============================================================================================
  // TO NESTINGS AND COMPLEXES
  //

  def toNesting[A, N <: Nat](n: N)(sc: StableCell[A]): Option[Nesting[Option[A], N]] = 
    sc.canopy match {
      case None => Some(Nesting.external(n)(sc.label))
      case Some(cn) => 
        for {
          nstSt <- cn.traverse(toNesting(n)(_))
          nstCn <- nstSt.unstableOfDim(n)
        } yield Box(sc.label, nstCn)

    }


}
