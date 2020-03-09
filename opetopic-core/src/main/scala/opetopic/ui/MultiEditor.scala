/**
  * MultiEditor.scala - An editor for Multitopes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

class MultiEditor[A, F <: ActiveFramework](val frmwk: F)(implicit rn: Renderable[A, F]) { thisMultiEditor => 

  type FT = frmwk.type
  type MultiCell[B] = LevelEditor[B]#LevelNeutralCell

  import frmwk._
  import isNumeric._

  //============================================================================================
  // INNER EDITOR
  //
  
  val innerControlEditor = new LevelEditor[A](SCardinal()) {
    
    override def extrudeSelectionWith(tgtVal: LabelType, fillVal: LabelType): Option[(SCardAddr, STree[Int])] =
      super.extrudeSelectionWith(tgtVal, fillVal).map({
        case (addr, msk) => {

          dblEditor.cardinal.traverseCardinal[Option,Unit]((cell: dblEditor.NeutralCell) => {
            for {
              cellEditor <- cell.label
              u <- cellEditor.extrudeAtAddrWithMask(None, None)(addr, msk)
            } yield { cell.layoutLabel }
          })

          dblEditor.renderAll

          (addr, msk)

        }
      })
    
    override def loopAtSelectionWith(tgtVal: LabelType, fillVal: LabelType) : Option[SCardAddr] =
      super.loopAtSelectionWith(tgtVal, fillVal).flatMap({
        case addr => {

          dblEditor.cardinal.traverseCardinal[Option,Unit]((cell: dblEditor.NeutralCell) => {
            for {
              cellEditor <- cell.label
              u <- cellEditor.loopAtAddrWith(None, None)(addr)
            } yield { cell.layoutLabel }
          })

          dblEditor.renderAll

          Some(addr)

        }
      })

    override def sproutAtSelectionWith(srcVal: LabelType, fillVal: LabelType): Option[SCardAddr] =
      super.sproutAtSelectionWith(srcVal, fillVal).flatMap({
        case addr => {

          dblEditor.cardinal.traverseCardinal[Option,Unit]((cell: dblEditor.NeutralCell) => {
            for {
              cellEditor <- cell.label
              u <- cellEditor.sproutAtAddrWith(None, None)(addr)
            } yield { cell.layoutLabel }
          })

          dblEditor.renderAll

          Some(addr)

        }
      })

  }

  //============================================================================================
  // OUTER EDITOR
  //

  val outerControlEditor = new LevelEditor[A](SCardinal()) {

    def extrusionData: (LevelEditor[A], LevelEditor[A]) = {

      val initCard: SCardinal[Option[A]] =
        innerControlEditor.cardinal.map(_.label)

      (createNestedEditor(initCard), createNestedEditor(initCard))

    }

    override def extrudeSelectionWith(tgtVal: LabelType, fillVal: LabelType): Option[(SCardAddr, STree[Int])] =
      super.extrudeSelectionWith(tgtVal, fillVal).flatMap({
        case (addr, msk) => {
          val (tgtEd, fillEd) = extrusionData
          dblEditor.extrudeAtAddrWithMask(Some(tgtEd), Some(fillEd))(addr, msk)
        }
      })

    override def loopAtSelectionWith(tgtVal: LabelType, fillVal: LabelType) : Option[SCardAddr] =
      super.loopAtSelectionWith(tgtVal, fillVal).flatMap({
        case addr => {
          val (tgtEd, fillEd) = extrusionData
          dblEditor.loopAtAddrWith(Some(tgtEd), Some(fillEd))(addr)
        }
      })

    override def sproutAtSelectionWith(srcVal: LabelType, fillVal: LabelType): Option[SCardAddr] =
      super.sproutAtSelectionWith(srcVal, fillVal).flatMap({
        case addr => {
          val (tgtEd, fillEd) = extrusionData
          dblEditor.sproutAtAddrWith(Some(tgtEd), Some(fillEd))(addr)
        }
      })

  }

  //============================================================================================
  // DOUBLE EDITOR
  //

  val dblEditor: LevelEditor[LevelEditor[A]] =
    new LevelEditor[LevelEditor[A]](
      SCardinal(Some(createNestedEditor(SCardinal())))
    ) { 

      override def createNeutralCell(
        dim: Int, lopt: Option[LevelEditor[A]], isExternal: Boolean
      ) : LevelNeutralCell = {

        lopt.foreach(le => {
          le.selectAfterExtrude = false
          le.refreshEdges
          le.refreshAddresses
          le.renderAll
        })

        val cell = new LevelNeutralCell(dim, lopt, isExternal)
        lopt.foreach(le => { le.parentCell = Some(cell) })

        cell

      }

      selectionEnabled = false

    }


  // Inner editor has customized hovering
  def createNestedEditor(card: SCardinal[Option[A]]): LevelEditor[A] =
    new LevelEditor[A](card) { thisEditor => 

      override def onHoverCell(cell: LevelNeutralCell): Unit = 
        for {
          pc <- thisEditor.parentCell
          z <- dblEditor.seekToAddress(pc.cardinalAddress)
          face <- z.focus.baseValue.face
        } {

          val cellAddr = cell.cardinalAddress

          face.foreach(edOpt => {
            for {
              ed <- edOpt
              zl <- ed.seekToAddress(cellAddr)
            } {
              val innerCell = zl.focus.baseValue
              innerCell.doHoverFaces
              innerCell.onHoverEdge
            }
          })

        }

      override def onUnhoverCell(cell: LevelNeutralCell): Unit = 
        for {
          pc <- thisEditor.parentCell
          z <- dblEditor.seekToAddress(pc.cardinalAddress)
          face <- z.focus.baseValue.face
        } {

          val cellAddr = cell.cardinalAddress

          face.foreach(edOpt => {
            for {
              ed <- edOpt
              zl <- ed.seekToAddress(cellAddr)
            } {
              val innerCell = zl.focus.baseValue
              innerCell.doUnhoverFaces
              innerCell.onUnhoverEdge
            }
          })

        }
      
    }
  
  //============================================================================================
  // LEVEL EDITOR IMPLEMENTATION
  //

  object LevelEditor {

    // Bingo!  We've got our recursive instance of renderability!
    implicit def levelEditorRenderable[B](implicit rn: Renderable[B, FT]): Renderable[LevelEditor[B], FT] =
      new Renderable[LevelEditor[B], FT] {
        def render(fm: FT)(le: LevelEditor[B]): fm.CellRendering = {
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

    var parentCell : Option[MultiCell[LevelEditor[B]]] = None

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
    // CUSTOM HOVERING
    //

    def onHoverCell(cell: LevelNeutralCell): Unit = {
      if (hoverCofaces)
        cell.doHoverCofaces

      cell.doHoverFaces
      cell.onHoverEdge
    }

    def onUnhoverCell(cell: LevelNeutralCell): Unit = {
      if (hoverCofaces) 
        cell.doUnhoverCofaces

      cell.doUnhoverFaces
      cell.onUnhoverEdge
    }

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
    ) extends LevelCell with MutableNeutralCell { thisNeutralCell => 

      def layoutLabel: Unit = {
        cellRendering = renderer.render(framework)(Neutral(label))
        labelNeedsLayout = false
      }

      private var myLabel: Option[B] = initLabel

      var cellRendering: CellRendering =
        renderer.render(framework)(Neutral(label))

      def label: Option[B] = myLabel
      def label_=(opt: Option[B]): Unit = {
        myLabel = opt
        labelNeedsLayout = true
      }

      override def onMouseOver: Unit = {
        onHoverCell(thisNeutralCell)
      }

      override def onMouseOut: Unit = {
        onUnhoverCell(thisNeutralCell)
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

