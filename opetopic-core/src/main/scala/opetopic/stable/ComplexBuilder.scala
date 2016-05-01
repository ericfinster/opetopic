/**
  * ComplexBuilder.scala - An abstract complex import/export class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Traverse
import scalaz.Applicative
import scalaz.syntax.traverse._
import scalaz.std.option._

import opetopic._
import syntax.tree._
import syntax.nesting._
import syntax.complex._

abstract class ComplexBuilder[A, C <: Cell[A, C]] {

  type OptA[N <: Nat] = Option[A]

  def newCell(opt: Option[A]): C
  def newCell(opt: Option[A], d: Nat): C

  def registerBaseCell(cell: C) : Unit

  def fromBox[N <: Nat](b: Box[Option[A], N]): Nesting[C, N] = 
    b match {
      case Box(a, cn) => {

        val n = cn.dim
        val cell = newCell(a, n)
        val newCn = cn.map(fromNesting(_))

        val canopy = STree(n)(newCn.map((nn: Nesting[C, N]) => {
          nn.baseValue.container = Some(cell)
          nn.baseValue
        }))

        cell.canopy = Some(canopy)
        Box(cell, newCn)

      }
    }

  @natElim
  def fromNesting[N <: Nat](n: N)(nst: Nesting[Option[A], N]) : Nesting[C, N] = {
    case (Z, Obj(o)) => Obj(newCell(o))
    case (Z, b @ Box(a, cn)) => fromBox(b)
    case (S(p), Dot(o, _)) => Dot(newCell(o, S(p)), S(p))
    case (S(p), b @ Box(a, cn)) => fromBox(b)
  }

  def fromNesting[N <: Nat](nst: Nesting[Option[A], N]) : Nesting[C, N] = 
    fromNesting(nst.dim)(nst)

  def toNesting[N <: Nat](n: N)(sc: C): Option[Nesting[Option[A], N]] = 
    sc.canopy match {
      case None => Some(Nesting.external(n)(sc.label))
      case Some(cn) => 
        for {
          nstSt <- cn.traverse(toNesting(n)(_))
          nstCn <- nstSt.unstableOfDim(n)
        } yield Box(sc.label, nstCn)

    }

  // A routine for passing down the source and target information
  def setupInternals(c: C, lvs: STree[C]): Option[C] =
    c.canopy match {
      case None => c.target  // When external, return the root
      case Some(cn) => {

        c.sourceTree = Some(lvs)

        for {
          myRoot <- cn.graftRec[C]({
            case addr => lvs.elementAt(addr)
          })({
            case (sc, tr) => {
              for {
                root <- setupInternals(sc, tr)
              } yield root
            }
          })
          _ = c.target = Some(myRoot)
        } yield myRoot

      }
    }

  def bond[P <: Nat](m: Nesting[C, P], n: Nesting[C, S[P]]) : ShapeM[Unit] = 
    (m, n) match {
      case (Box(pcell, pcn), Box(cell, cn)) => {
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
          lvs <- fromOpt(m.baseValue.spine)
          _ <- fromOpt(setupInternals(cell, lvs))
        } yield ()
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

    @natElim
    def fromComplex[N <: Nat](n: N)(c: Complex[OptA, N]): ShapeM[Nesting[C, N]] = {
      case (Z, Complex(_, objs)) => {
        val no = fromNesting(objs)
        registerBaseCell(no.baseValue)
        succeed(no)
      }
      case (S(p: P), Complex(tl, hd)) => {
        for {
          blorp <- fromComplex(p)(tl)
          bleep = fromNesting(hd)
          _ <- bond(blorp, bleep)
          _ = registerBaseCell(bleep.baseValue)
        } yield bleep
      }
    }

    @natElim
    def toComplex[N <: Nat](n: N)(sc: C): Option[Complex[OptA, N]] = {
      case (Z, sc) => {
        // println("Writing objects ...")
        for {
          objNst <- toNesting(Z)(sc)
          // _ = println("done")
        } yield Complex[OptA] >> objNst
      }
      case (S(p: P), sc) => {
        // println("Writing dimension " + natToInt(S(p)).toString)
        for {
          tgt <- sc.target
          // _ = println("Got target")
          tl <- toComplex(p)(tgt)
          // _ = println("Got tail")
          hd <- toNesting(S(p))(sc)
          // _ = println("Got head")
        } yield tl >> hd
      }

    }

}


