/**
  * Cell.scala - Mutable opetopic cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mutable

trait Cell[A] {

  type CellTree = PTree[Cell[A]]

  def dim: Int
  def label: Option[A]

  //
  // Cell attributes
  //

  def canopy: Option[CellTree]
  def container: Option[Cell[A]]

  def target : Option[Cell[A]]
  def sourceTree: Option[CellTree]

  def isBase: Boolean = 
    container == None

  def isDrop: Boolean = 
    sourceTree == None

  def isExternal: Boolean = 
    canopy == None

  // 
  // Edge attributes
  //

  def incoming: Option[Cell[A]]
  def outgoing: Option[Cell[A]]

}

