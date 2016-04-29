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
      case (m, Box(a, cn)) => 
        for {
          sp <- Nesting.spineFromCanopy(cn)
          _ <- Tree.matchTraverse(m.toTree, sp)({
            case (sc0, sc1) => {

              // Setup remaing bond information

              sc1.sourceTree = sc0.canopy
              sc1.target = Some(sc0)

              sc0.incoming = Some(sc1)
              sc0.canopy.map(_.map(c => { c.outgoing = Some(sc1) }))

              succeed(())

            }
          })
        } yield ()
      case (Box(t, cn), Dot(c, dm)) => {

        c.sourceTree = t.canopy
        c.target = Some(t)

        cn.map(s => { s.baseValue.outgoing = Some(c) })
        t.incoming = Some(c)

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
