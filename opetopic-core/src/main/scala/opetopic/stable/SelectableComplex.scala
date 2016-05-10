/**
  * SelectableComplex.scala - Cells with selection capabilities
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scala.collection.mutable.Buffer

trait SelectableComplex[A] {

  type CellType <: SelectableCell

  val selectedCells: Buffer[CellType] = Buffer.empty
  var selectionRoot: Option[CellType] = None

  def deselectAll: Unit = {
    selectedCells.foreach(_.deselect)
    selectedCells.clear
  }

  trait SelectableCell extends Cell[A, CellType] { thisCell : CellType => 

    def canSelect: Boolean
    var isSelected: Boolean

    def selectAsRoot: Unit = 
      if (canSelect) {
        deselectAll
        isSelected = true
        selectionRoot = Some(thisCell)
      }

    def select: Unit = 
      if (canSelect && ! isSelected) {

        val buf: Buffer[CellType] = Buffer(thisCell)
        var pOpt: Option[CellType] = parent
        var found: Boolean = false

        while (pOpt != None && ! found) {
          val p = pOpt.get
          if (p.isSelected) {
            found = true
          } else {
            buf += p
            pOpt = p.parent
          }
        }

        if (found) {
          buf.foreach(cell => {
            cell.isSelected = true
            selectedCells += cell
          })
        } else selectAsRoot

      }

    def deselect: Unit = {
      isSelected = false
    }

  }

}
