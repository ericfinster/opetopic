/**
  * Expr.scala - Expressions for Makkai-style type theory
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mtt

import opetopic._
import opetopic.ui._

sealed trait Expr
case class Var(nm: String) extends Expr
case class Comp(pd: STree[Expr]) extends Expr
case class Frame(frm: SComplex[Expr]) extends Expr
case class Expand(fa: FaceAddr) extends Expr
case class Contract(fa: FaceAddr) extends Expr

// Right, so we have to work out a couple of final details
// about how this should work.

// You should have, I guess, a way of composing pasting diagrams
// to make composites.

// I am wondering if we should keep the treatment of identities
// separate from that of other composites.  The fact is that
// The insertion of an identity requires slightly differend addressing,
// no?  Well, not exactly.  I guess you can make it uniform.

// So perhaps the first thing is to set up the infrastructure of how
// to create variables and composites and make sure that the face
// modification routines work as planned.  We also need a method for
// selecting how to insert identities....

// Right.  So first step should be getting a simple page up and
// going with the ability to make the kinds of transformations that
// you're interested in.  Let's see if we can make this kind of thing
// happen so that we have something to play with.

case class ExprMarker(val expr: Expr)

object ExprMarker {

  implicit object ExprMarkerRenderable extends Renderable[ExprMarker] {
    def render(f: UIFramework)(mk: ExprMarker): f.CellRendering = {

      import f._
      import isNumeric._

      // implicit def intToUnit(i: Int) : Size =
      //   fromInt(i)

      // val lblEl = 
      //   if (mk.displayName == "")
      //     spacer(Bounds(0, 0, 600, 600))
      //   else text(mk.displayName)

      // val td = mk.targetDec.map(_.render(f))
      // val sd = mk.sourceDec.mapValues(_.render(f))

      CellRendering(text(mk.expr.toString))

    }
  }

}


