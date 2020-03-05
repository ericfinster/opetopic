/**
  * MultiEditor.scala - An editor for Multitopes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

class MultiEditor[A, F <: ActiveFramework](val frmwk: F)(implicit rn: Renderable[A, F]) {

  type FT = frmwk.type

  import frmwk._
  import isNumeric._

  val innerControlEditor = new LevelEditor[A](SCardinal())
  val outerControlEditor = new LevelEditor[A](SCardinal())

  val dblEditor: LevelEditor[LevelEditor[A]] =
    new LevelEditor[LevelEditor[A]](
      SCardinal(Some(
        new LevelEditor[A](SCardinal())
      )))

  //============================================================================================
  // LEVEL EDITOR IMPLEMENTATION
  //

  object LevelEditor {

    // Bingo!  We've got our recursive instance of renderability!
    implicit def levelEditorRenderable[B](implicit rn: Renderable[B, FT]): Renderable[LevelEditor[B], FT] =
      new Renderable[LevelEditor[B], FT] {
        def render(fm: FT)(le: LevelEditor[B]): fm.CellRendering = {
          le.renderAll
          CellRendering(BoundedElement(le.galleryGroup, le.groupBounds))
        }
      }

  }

  class LevelEditor[B](c: SCardinal[Option[B]])(implicit rn: Renderable[B, FT])
    extends ActiveStableGallery[FT](frmwk)
      with MutableCardinalGallery[FT] {

    type LabelType = Option[B]
    type PanelType = LevelPanel
    type SelectionType = LevelCell

    type CellType = LevelCell
    type NeutralCellType = LevelNeutralCell
    type PolarizedCellType = LevelPolarizedCell
    type PositiveCellType = LevelPositiveCell
    type NegativeCellType = LevelNegativeCell

    type OptB = Option[B]
    type PolOptB = Polarity[Option[B]]

    def defaultLabel: Option[B] = None

    val renderer : Renderable[PolOptB, FT] =
      Renderable[PolOptB, FT]

    //============================================================================================
    // EDITOR DATA
    //

    var panels : Suite[LevelPanel] = buildPanels(c)._1

    def cardinal: SCardinal[LevelNeutralCell] =
      Traverse[Suite].map(panels)(_.cardinalNesting)

    def cardinal_=(c: SCardinal[OptB]): Unit = {
      panels = buildPanels(c)._1
    }

    //============================================================================================
    // CONSTRUCTORS
    //

    // Panel Constructor
    def createPanel(dim: Int, cn: SCardNst[NeutralCellType], ed: Either[PanelType, SNesting[CellType]]): PanelType =
      new LevelPanel(dim, cn, ed)

    // Neutral Cell Constructor
    def createNeutralCell(dim: Int, initLabel: LabelType, isExternal: Boolean) : NeutralCellType =
      new LevelNeutralCell(dim, initLabel, isExternal)

    //============================================================================================
    // SELECTION SEEKING
    //

    def seekToAddress(addr: SCardAddr): Option[SNstZipper[LevelCell]] =
      cardinal.seekNesting(addr)

    def seekToCanopy(addr: SCardAddr): Option[SZipper[SNesting[LevelCell]]] =
      cardinal.seekCanopy(addr)

    //============================================================================================
    // PANEL IMPLEMENTATION
    //

    class LevelPanel(
      val dim: Int,
      var cardinalNesting: SCardNst[LevelNeutralCell],
      val edgeData: Either[LevelPanel, SNesting[LevelCell]]
    ) extends ActiveStablePanel
        with MutableCardinalPanel {

      val positiveCell: PositiveCellType = new LevelPositiveCell(dim)
      val negativeCell: NegativeCellType = new LevelNegativeCell(dim)

    }

    //============================================================================================
    // CELL IMPLEMENTATIONS
    //

    abstract class LevelCell extends ActiveCell with MutableCardinalCell

    class LevelNeutralCell(
      val dim: Int,
      initLabel: Option[B],
      var isExternal: Boolean
    ) extends LevelCell with MutableNeutralCell {

      def layoutLabel: Unit = {
        cellRendering = renderer.render(framework)(Neutral(label))
      }

      private var myLabel: Option[B] = initLabel

      var cellRendering: CellRendering =
        renderer.render(framework)(Neutral(label))

      def label: Option[B] = myLabel
      def label_=(opt: Option[B]): Unit = {
        myLabel = opt
        labelNeedsLayout = true
      }

    }

    abstract class LevelPolarizedCell extends LevelCell with MutablePolarizedCell {

      override def isVisible = false
      override def onClick: Unit = deselectAll
      override def onMouseOver: Unit = ()
      override def onMouseOut: Unit = ()
      override def onHover: Unit = ()
      override def onUnhover: Unit = ()

    }

    class LevelPositiveCell(val dim: Int) extends LevelPolarizedCell with PositiveCell {

      var label: Option[B] = None
      var isExternal: Boolean = false

      val cellRendering: CellRendering =
        renderer.render(framework)(Positive())

      override def toString = "+"

    }

    class LevelNegativeCell(val dim: Int) extends LevelPolarizedCell with NegativeCell {

      var label: Option[B] = None
      var isExternal: Boolean = true

      val cellRendering: CellRendering =
        renderer.render(framework)(Negative())

      override def toString = "-"

    }

  }
  

}

