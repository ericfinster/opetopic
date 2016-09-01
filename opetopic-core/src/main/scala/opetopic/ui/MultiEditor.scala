/**
  * MultiEditor.scala - An editor for Multitopes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

class MultiEditor[A: Renderable, F <: ActiveFramework](frmwk: F) {

  type OptA = Option[A]

  sealed trait Multicard[+A]
  case class Root[+A](card: SCardinal[A]) extends Multicard[A]
  case class Level[+A](cmpl: SComplex[Multicard[A]]) extends Multicard[A]

  // Great, so if we have a multitope of these guys, we can collapse
  // it down.  This should be what happens when we finish a level edit.
  def stack(m: Multitope[Multicard[A]]): Multicard[A] =
    m match {
      case Base(c) => Level(c)
      case Up(c) => Level(c.map(stack(_)))
    }

  // Great, so this lifts the multicardinal structure up.
  def splitLevel(m: Multicard[A], i: Int): Option[Multicard[Multicard[A]]] =
    m match {
      case Root(card) => None
      case Level(cmplx) =>
        if (i == 0) Some(Root(SCardinal(cmplx))) else {
          for {
            childCmplx <- cmplx.traverse(splitLevel(_, i-1))
          } yield Level(childCmplx)
        }
    }

  // This means we can apply, say, an extrusion to each of the cardinals
  // at the leaves of our multicardinal.
  def traverseRoots(m: Multicard[A])(f: SCardinal[A] => Option[SCardinal[A]]): Option[Multicard[A]] =
    m match {
      case Root(card) => f(card).map(Root(_))
      case Level(cmplx) => cmplx.traverse(traverseRoots(_)(f)).map(Level(_))
    }

}

