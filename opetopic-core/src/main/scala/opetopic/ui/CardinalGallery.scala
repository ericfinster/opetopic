/**
  * CardinalGallery.scala - A Gallery for displaying a Cardinal
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

trait CardinalGallery[F <: UIFramework]
    extends StableGallery[F] {

  type PanelType <: CardinalPanel
  type CellType <: CardinalCell

  type NeutralCellType <: CellType with NeutralCell
  type PolarizedCellType <: CellType with PolarizedCell
  type PositiveCellType <: CellType with PositiveCell
  type NegativeCellType <: CellType with NegativeCell
  
  type AddressType = SCardAddr

  // The currently displayed cardinal
  def cardinal: SCardinal[NeutralCellType] 

  // The complex calculated from the current cardinal
  def complex: SComplex[CellType] = 
    Traverse[Suite].map(panels)(_.boxNesting)

  // Panel creation uses default labels
  def defaultLabel: LabelType
  
  //============================================================================================
  // CARDINAL PANELS
  //

  trait CardinalPanel extends StablePanel { thisPanel : PanelType => 

    def cardinalNesting: SCardNst[NeutralCellType]
    def edgeData: Either[PanelType, SNesting[CellType]]

    def positiveCell: PositiveCellType
    def negativeCell: NegativeCellType

    def boxNesting: SNesting[CellType] = 
      cardinalNesting.toNesting(
        positiveCell, 
        negativeCell
      )

    def edgeNesting: SNesting[CellType] = 
      edgeData match {
        case Left(pp) => pp.boxNesting
        case Right(en) => en
      }


  }

  // Panel Constructor
  def createPanel(dim: Int, cn: SCardNst[NeutralCellType], ed: Either[PanelType, SNesting[CellType]]): PanelType
  
  //============================================================================================
  // CARDINAL CELLS
  //

  trait CardinalCell extends GalleryCell { thisCell : CellType => 

    def cardinalAddress: SCardAddr
    def address = cardinalAddress.complexAddress
    
  }

  // Neutral Cell Constructor
  def createNeutralCell(dim: Int, initLabel: LabelType, isExternal: Boolean) : NeutralCellType

  trait NeutralCell extends CardinalCell { thisCell : CellType with NeutralCellType => }
  trait PolarizedCell extends CardinalCell { thisCell : CellType with PolarizedCellType => }

  trait PositiveCell extends PolarizedCell { thisCell : CellType with PolarizedCellType with PositiveCellType => }
  trait NegativeCell extends PolarizedCell { thisCell : CellType with PolarizedCellType with NegativeCellType => }

  //============================================================================================
  // INITIALIZATION
  //

  def buildPanels(c: SCardinal[LabelType]): (Suite[PanelType], Int) = 
    c match {
      case ||(cn) => {
        val bn = Traverse[MTree].map(cn)(buildCells(0, _))

        // Hmmm.  You should find a way to not have to expose
        // a default value here.  It's a bit ugly....
        val inEdge = createNeutralCell(-1, defaultLabel, true)
        val outEdge = createNeutralCell(-1, defaultLabel, false)
        val en = SBox(outEdge, STree.obj(SDot(inEdge)))

        (||(createPanel(0, bn, Right(en))), 0)

      }
      case tl >> hd => {
        val (pt, d) = buildPanels(tl)
        val bn = Traverse[MTree].map(hd)(buildCells(d + 1, _))
        (pt >> createPanel(d + 1, bn, Left(pt.head)), d + 1)
      }
    }

  // Here we should take an address prefix as well and store
  // the actual cardinal address...
  def buildCells(d: Int, n: SNesting[LabelType]): SNesting[NeutralCellType] = 
    n.foldNestingWithAddr[SNesting[NeutralCellType]]()({
      case (optLabel, addr) => SDot(createNeutralCell(d, optLabel, true))
    })({
      case (optLabel, addr, cn) => SBox(createNeutralCell(d, optLabel, false), cn)
    })

}
