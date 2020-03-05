/**
  * SimpleStableEditor.scala - A Stable Cardinal Editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

class SimpleStableEditor[A, F <: ActiveFramework](frmwk: F)(c: SCardinal[Option[A]])(implicit rn: Renderable[A, F])
    extends ActiveStableGallery[F](frmwk)
    with MutableCardinalGallery[F] {

  import framework._
  import isNumeric._

  type LabelType = Option[A]
  type PanelType = SimplePanel
  type SelectionType = SimpleCell

  type CellType = SimpleCell
  type NeutralCellType = SimpleNeutralCell
  type PolarizedCellType = SimplePolarizedCell
  type PositiveCellType = SimplePositiveCell
  type NegativeCellType = SimpleNegativeCell

  type OptA = Option[A]
  type PolOptA = Polarity[Option[A]]

  def defaultLabel: Option[A] = None

  val renderer : Renderable[PolOptA, F] = 
    Renderable[PolOptA, F]

  //============================================================================================
  // EDITOR DATA
  //

  var panels : Suite[SimplePanel] = buildPanels(c)._1

  def cardinal: SCardinal[SimpleNeutralCell] =
    Traverse[Suite].map(panels)(_.cardinalNesting)

  def cardinal_=(c: SCardinal[OptA]): Unit = {
    panels = buildPanels(c)._1
  }

  //============================================================================================
  // CONSTRUCTORS
  //

  // Panel Constructor
  def createPanel(dim: Int, cn: SCardNst[NeutralCellType], ed: Either[PanelType, SNesting[CellType]]): PanelType =
    new SimplePanel(dim, cn, ed)

  // Neutral Cell Constructor
  def createNeutralCell(dim: Int, initLabel: LabelType, isExternal: Boolean) : NeutralCellType =
    new SimpleNeutralCell(dim, initLabel, isExternal)

  //============================================================================================
  // SELECTION SEEKING
  //

  // The original StableEditor uses the more specific type of SimpleNeutralCell.  This
  // does not compile for the moment, but perhaps this can be fixed?

  def seekToAddress(addr: SCardAddr): Option[SNstZipper[SimpleCell]] = 
    cardinal.seekNesting(addr)

  def seekToCanopy(addr: SCardAddr): Option[SZipper[SNesting[SimpleCell]]] = 
    cardinal.seekCanopy(addr)

  //============================================================================================
  // PANEL IMPLEMENTATION
  //

  class SimplePanel(
    val dim: Int,
    var cardinalNesting: SCardNst[SimpleNeutralCell],
    val edgeData: Either[SimplePanel, SNesting[SimpleCell]]
  ) extends ActiveStablePanel
      with MutableCardinalPanel {

    val positiveCell: PositiveCellType = new SimplePositiveCell(dim)
    val negativeCell: NegativeCellType = new SimpleNegativeCell(dim)

  }

  //============================================================================================
  // CELL IMPLEMENTATIONS
  //

  abstract class SimpleCell extends ActiveCell with MutableCardinalCell

  class SimpleNeutralCell(
    val dim: Int,
    initLabel: Option[A], 
    var isExternal: Boolean
  ) extends SimpleCell with MutableNeutralCell {

    def layoutLabel: Unit = {
      cellRendering = renderer.render(framework)(Neutral(label))
      makeMouseInvisible(labelElement)
    }

    private var myLabel: Option[A] = initLabel

    var cellRendering: CellRendering = 
      renderer.render(framework)(Neutral(label))
    
    def label: Option[A] = myLabel
    def label_=(opt: Option[A]): Unit = {
      myLabel = opt
      labelNeedsLayout = true
    }

    makeMouseInvisible(labelElement)

  }

  abstract class SimplePolarizedCell extends SimpleCell with MutablePolarizedCell {
    override def isVisible = false
    override def onClick: Unit = deselectAll
    override def onMouseOver: Unit = ()
    override def onMouseOut: Unit = ()
    override def onHover: Unit = ()
    override def onUnhover: Unit = ()
  }

  class SimplePositiveCell(val dim: Int) extends SimplePolarizedCell with PositiveCell {

    var label: Option[A] = None
    var isExternal: Boolean = false

    val cellRendering: CellRendering = 
      renderer.render(framework)(Positive())

    override def toString = "+"

  }

  class SimpleNegativeCell(val dim: Int) extends SimplePolarizedCell with NegativeCell {

    var label: Option[A] = None
    var isExternal: Boolean = true

    val cellRendering: CellRendering = 
      renderer.render(framework)(Negative())

    override def toString = "-"

  }

}
