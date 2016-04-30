/**
  * SimpleCell.scala - A Simple Cell Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mutable

class SimpleCell[A] extends Cell[A] {

  def this(a: A) = {
    this
    label = Some(a)
  }

  def this(a: A, d: Int) = {
    this
    label = Some(a)
    dim = d
  }

  type STree = PTree[SimpleCell[A]]

  var dim: Int = 0
  var label: Option[A] = None

  var canopy: Option[STree] = None
  var container: Option[SimpleCell[A]] = None

  var target: Option[SimpleCell[A]] = None
  var sourceTree: Option[STree] = None

  var incoming: Option[SimpleCell[A]] = None
  var outgoing: Option[SimpleCell[A]] = None

  //============================================================================================
  // FACE OPERATIONS
  //

  import scalaz.Traverse
  import scalaz.std.option._
  import scalaz.syntax.traverse._

  def spine: Option[STree] = 
    canopy match {
      case None => {
        for {
          st <- sourceTree
        } yield PNode(this, st.nodes.map(_ => PLeaf))
      }
      case Some(cn) => 
        for {
          jn <- cn.traverse(_.spine)
        } yield PTree.join(jn)
    }

  def partialSpine(guide: STree) : Option[STree] = 
    (guide, canopy) match {
      case (_, None) => 
        for {
          st <- sourceTree
        } yield PNode(this, st.nodes.map(_ => PLeaf))
      case (PLeaf, Some(cn)) => // Do the same?
        for {
          st <- sourceTree
        } yield PNode(this, st.nodes.map(_ => PLeaf))
      case (PNode(_, bs), Some(cn)) => {

        // Right, and now here you see a problem:  the list
        // of branches is flat, but the canopy is not.  How
        // do you match les unes avec les autres?

        // Wait .... wait .... use the canopy of the embedded
        // cell .... or something ....

        // Ah, no, it's an external cell.  Right, but it's
        // source tree is how the thing was constructed in
        // the first place.

        // This suggests: zip the source tree 

        ???
      }
    }

  // Okay, but I'm not sure we need this.  Rather, if you want to 
  // compute the face, you take the spine and pass it to the target.
  // Right, that's the key.  Take the spine, pass it to the target.
  // Take the spine, pass it to the target.

  // Now, when you receive a spine from the previous dimension,
  // the idea is that it is a message of what to contract.  Is 
  // this right?

  // No, I see.  The point is that you should do a kind of
  // *relative* spine where you use go only as high as is 
  // specified by what you were given.  Right.  

  def extract(st: STree) : Option[SimpleCell[A]] = 
    target match {
      case None => {

        // So, now, you've reached an object and you've
        // got the tree there.  You basically now traverse
        // it, ripping out the targets and sources of the
        // remaining guys and gluing them all together.

        ???

      }
      case Some(tgt) => {
        for {
          sp <- partialSpine(st)
          res <- tgt.extract(sp)
        } yield res
      }
    }

}

object SimpleCell {

  import opetopic._
  import syntax.tree._
  import syntax.nesting._
  import syntax.complex._

  def fromNesting[A, N <: Nat](nst: Nesting[A, N]) : Nesting[SimpleCell[A], N] = 
    nst match {
      case Obj(a) => Obj(new SimpleCell(a))
      case Dot(a, d) => Dot(new SimpleCell(a, natToInt(d)), d)
      case Box(a, cn) => {

        val n = cn.dim
        val cell = new SimpleCell(a, natToInt(n))
        val newCn = cn.map(fromNesting(_))
        val canopy = PTree(n)(newCn.map((nn: Nesting[SimpleCell[A], N]) => {
          nn.baseValue.container = Some(cell)
          nn.baseValue
        }))

        cell.canopy = Some(canopy)
        Box(cell, newCn)

      }
    }


  def bond[A, P <: Nat](m: Nesting[SimpleCell[A], P], n: Nesting[SimpleCell[A], S[P]]) : ShapeM[Unit] = 
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

    type ConstA[N <: Nat] = A

    @natElim
    def fromComplex[N <: Nat](n: N)(c: Complex[ConstA, N]): ShapeM[Nesting[SimpleCell[A], N]] = {
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
