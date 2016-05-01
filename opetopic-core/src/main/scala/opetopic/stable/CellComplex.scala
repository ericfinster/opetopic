/**
  * CellComplex.scala - Cell Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scala.collection.mutable.ListBuffer

abstract class CellComplex[A, C <: Cell[A, C]] extends ComplexBuilder[A, C] {

  // Here is a buffer for containing
  // the list of the base cells.  For now,
  // I think that's basically all I want. 


  val baseCells: ListBuffer[C] = ListBuffer()


}
