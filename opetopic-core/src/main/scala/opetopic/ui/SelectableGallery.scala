/**
  * SelectableGallery.scala - Galleries with selectable cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scala.collection.mutable.Buffer

import opetopic._

trait SelectableGallery {

  type AddressType
  type SelectionType <: SelectableCell

  var onSelectAsRoot: SelectionType => Unit = { _ => () }

  val selectedCells: Buffer[SelectionType] = Buffer.empty
  var selectionRoot: Option[SelectionType] = None

  var selectionEnabled: Boolean = true

  def deselectAll: Unit = {
    selectedCells.foreach(_.deselect)
    selectedCells.clear
    selectionRoot = None
  }

  def seekToCanopy(addr: AddressType): Option[SZipper[SNesting[SelectionType]]]

  trait SelectableCell { thisCell : SelectionType => 

    def selectionAddress: AddressType

    def onSelected: Unit
    def onDeselected: Unit

    def canSelect: Boolean
    var isSelected: Boolean = false

    def selectAsRoot: Unit = 
      if (selectionEnabled && canSelect) {
        deselectAll
        isSelected = true
        selectionRoot = Some(thisCell)
        selectedCells += thisCell
        onSelectAsRoot(thisCell)
        onSelected
      }

    def select: Unit = 
      if (selectionEnabled && canSelect && ! isSelected) {

        val buf: Buffer[SelectionType] = Buffer(thisCell)
        var found: Boolean = false

        for {
          zp <- seekToCanopy(selectionAddress)
          r <- zp.predecessorWhich(n => {
            buf += n.baseValue
            n.baseValue.isSelected
          })
        } { 

          for { b <- buf } {
            b.isSelected = true
            b.onSelected
            selectedCells += b
          }

          found = true 

        }

        if (! found) {
          selectAsRoot
        }

      }

    def deselect: Unit = {
      isSelected = false
      onDeselected
    }

  }

}
