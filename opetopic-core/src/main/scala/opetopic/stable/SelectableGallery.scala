/**
  * SelectableGallery.scala - Galleries with selectable cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scala.collection.mutable.Buffer

trait SelectableGallery {

  type AddressType
  type SelectionType <: SelectableCell

  val selectedCells: Buffer[SelectionType] = Buffer.empty
  var selectionRoot: Option[SelectionType] = None

  def deselectAll: Unit = {
    selectedCells.foreach(_.deselect)
    selectedCells.clear
    selectionRoot = None
  }

  def seekToAddress(addr: AddressType): Option[SNstZipper[SelectionType]]

  trait SelectableCell { thisCell : SelectionType => 

    def selectionAddress: AddressType

    def onSelected: Unit = ()
    def onDeselected: Unit = ()

    def canSelect: Boolean
    var isSelected: Boolean

    def selectAsRoot: Unit = 
      if (canSelect) {
        deselectAll
        isSelected = true
        selectionRoot = Some(thisCell)
        selectedCells += thisCell
        onSelected
      }

    def select: Unit = 
      if (canSelect && ! isSelected) {

        val buf: Buffer[SelectionType] = Buffer()
        var found: Boolean = false

        for {
          zp <- seekToAddress(selectionAddress)
          r <- zp.predecessorWhich(b => {
            buf += b
            b.isSelected
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
