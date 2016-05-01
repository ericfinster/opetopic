/**
  * SimpleCell.scala - A Simple Cell Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import opetopic._

class SimpleCell[A] extends Cell[A, SimpleCell[A]] {

  def this(opt: Option[A]) = {
    this
    label = opt
  }

  def this(opt: Option[A], d: Nat) = {
    this
    label = opt
    dim = natToInt(d)
  }

  var dim: Int = 0
  var label: Option[A] = None

  var canopy: Option[CellTree] = None
  var container: Option[SimpleCell[A]] = None

  var target : Option[SimpleCell[A]] = None
  var sourceTree: Option[CellTree] = None

  var incoming: Option[SimpleCell[A]] = None
  var outgoing: Option[SimpleCell[A]] = None

}

class SimpleComplexBuilder[A] extends ComplexBuilder[A, SimpleCell[A]] {

  def newCell(opt: Option[A]): SimpleCell[A] = new SimpleCell(opt)
  def newCell(opt: Option[A], d: Nat): SimpleCell[A] = new SimpleCell(opt, d)

  def registerBaseCell(cell: SimpleCell[A]): Unit = ()

}
