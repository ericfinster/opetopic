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
  // FACES, ETC.
  //

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

    // Okay, I'm not sure I go everything,  but at least you see
    // the main idea of this routine now: it copies a bond.

    guide match {
      case SLeaf => ???
      case SNode(c, cs) => {

        // We are the target cell.  Copy ourselves, etc.
        val targetCell = new StableCell[A]
        targetCell.label = label

        // The external cell is a copy of c, the node in our guide
        val externalCell = new StableCell[A]
        externalCell.label = c.label
        // Right, so on the one hand, we are supposed to
        // be copying the current cell, since this will be
        // the input and output.  On the other, we seem to
        // want to copy the cell c as the known space between
        // the two.

        // Probably we should do both simultaneously, since
        // what we are really doing is extracting a *bond*.

        // We use the canopy to zip up with the guide
        // and pass to the cells lying over us.  We are
        // going to use these for the canopy of the new
        // guy.  The thing is, I suppose, that we won't
        // end up with a tree, but just the base of the
        // new guy ...

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
      case (m, Box(_, cn)) => 
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
        } yield ()
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

  }

}
