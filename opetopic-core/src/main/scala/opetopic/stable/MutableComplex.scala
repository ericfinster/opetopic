/**
  * MutableComplex.scala - Cell Mutability Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

trait MutableComplex[A] {

  type CellType <: MutableCell

  trait MutableCell extends Cell[A, CellType] { thisCell: CellType => 


  }

}
