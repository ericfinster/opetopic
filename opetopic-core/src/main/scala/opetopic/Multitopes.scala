/**
  * Multitopes.scala - An implementation of multitopes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

sealed trait Multitope[+A]
case class Base[+A](c: SComplex[A]) extends Multitope[A]
case class Up[+A](c: SComplex[Multitope[A]]) extends Multitope[A]

object Multitope {

  def level[A](m: Multitope[A]): Int =
    m match {
      case Base(c) => 1
      case Up(c) => level(c.topValue) + 1
    }

  def dim[A](m: Multitope[A]): Int =
    m match {
      case Base(c) => c.dim
      case Up(c) => c.dim * dim(c.topValue)
    }

  // What now?  I guess the point is that you're going to need multi-cardinals
  // as well in order to write an editor.  This poses a slight problem.  If you
  // allows cardinals at all levels, then I don't think there's any kind of well
  // defined face algorithm: the face of a multi-cardinal won't be a collection
  // of complexes, but rather a collection of cardinals.

  // So this raises the question of how the editor should work.  Originally, I had
  // envisioned that *all* levels would be simultaneously editable.  But it seems
  // that this leads to a structure where every face is a cardinal, meaning then
  // that it becomes more difficult to see when this object is "closed" as a cell.

  // The alternative would be that the editor allows you to edit only one level at
  // a time.  In this case, everything remains a complex, but you can, at each point,
  // load that level into the editor an modify it, at which point each new cell
  // added to the editor gets extended with a "model multitope" which is calculated
  // at the time that the level is opened for editing.

  // One then closes the edit of a given level by selecting a well-defined cell
  // and then the zipper to that level is closed with the face selected.

  // That seems pretty reasonable.  Let's give that a try.


}

