/**
  * MutableCardinalGallery.scala - Mutation routines for a cardinal gallery
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

trait MutableCardinalGallery[F <: UIFramework]
    extends CardinalGallery[F] with SelectableGallery {

  import framework._
  import isNumeric._

  type CellType <: MutableCardinalCell
  type NeutralCellType <: CellType with MutableNeutralCell
  type PolarizedCellType <: CellType with MutablePolarizedCell
  type SelectionType <: MutableCardinalCell
  
  type PanelType <: MutableCardinalPanel

  var panels : Suite[PanelType] 

  //============================================================================================
  // GALLERY CONFIGURATION
  //

  //
  //  Visual Options
  //

  var internalPadding : Size = fromInt(400)
  var externalPadding : Size = fromInt(600)
  var decorationPadding : Size = fromInt(300)
  var leafWidth : Size = fromInt(200)
  var strokeWidth : Size = fromInt(100)
  var cornerRadius : Size = fromInt(200)

  //
  //  Gallery Options
  //

  var width: Size = fromInt(900)
  var height: Size = fromInt(300)
  var panelSpacing: Size = fromInt(2000)

  var layoutWidth: Bounds => Size = 
    (pb: Bounds) => width

  var layoutHeight: Bounds => Size = 
    (pb: Bounds) => height

  var layoutViewport: Bounds => Bounds = 
    (pb: Bounds) => pb

  var firstPanel: Option[Int] = None
  var lastPanel: Option[Int] = None

  //============================================================================================
  // REFRESH ROUTINES
  //

  def renderAll: Unit

  // These should be combined somehow ...
  def refreshEdges: Unit = 
    panels.foreach(_.refreshEdges)

  def refreshAddresses: Unit = {
    panels.foreach(_.refreshAddresses)
  }

  //============================================================================================
  // MUTABLE CARDINAL PANELS
  //

  trait MutableCardinalPanel extends CardinalPanel { thisPanel : PanelType => 

    var cardinalNesting: SCardNst[NeutralCellType]
    
    def refreshAddresses: Unit = 
      cardinalNesting.foreachWithAddr(
        (box, addr) => { box.cardinalAddress = addr }
      )

    def refreshEdges: Unit = 
      edgeData match {
        case Left(pp) => {

          boxNesting match {
            case SDot(c) => c.outgoingEdge = Some(pp.boxNesting.baseValue)
            case SBox(_, cn) => 
              for {
                sp <- cn.spine
                _ <- sp.matchTraverse[EdgeType, Unit](pp.boxNesting.toTree)({
                  case (c, e) => Some({ c.outgoingEdge = Some(e) })
                })
              } { }
          }
          
        }
        case Right(en) => {
          boxNesting.map(c => c.outgoingEdge = Some(en.baseValue))
        }
      }

    override def bounds: Bounds = {

      // Here are all the leaves
      val lvs = edgeNesting.spine(SDeriv(SLeaf)).getOrElse(SLeaf).toList

      // And here are all the boxes
      val bxs = boxNesting match {
        case SDot(_) => Nil // Shouldn't happen
        case SBox(_, cn) => cn.toList.map(_.baseValue).filter(_.isVisible)
      }

      val (boxMinX, boxMaxX, boxMinY, boxMaxY) =
        bxs match {
          case Nil => (zero,zero,zero,zero) // Error?
          case b::bs => {
            (bs foldLeft (b.x,b.x + b.width, b.y, b.y + b.height))({
              case ((curMinX, curMaxX, curMinY, curMaxY), cell) => {
                val nextMinX = isOrdered.min(curMinX, cell.x)
                val nextMaxX = isOrdered.max(curMaxX, cell.x + cell.width)
                val nextMinY = isOrdered.min(curMinY, cell.y)
                val nextMaxY = isOrdered.max(curMaxY, cell.y + cell.height)
                (nextMinX, nextMaxX, nextMinY, nextMaxY)
              }
            })
          }
        }

      // Now adjust if any of the leaves exceed the previous box calculations
      val (minX, maxX, minY) =
        (lvs foldLeft (boxMinX, boxMaxX, boxMinY))({
          case ((curMinX, curMaxX, curMinY), cell) => {
            val nextMinX = isOrdered.min(curMinX, cell.edgeStartX)
            val nextMaxX = isOrdered.max(curMaxX, cell.edgeStartX)
            val nextMinY = isOrdered.min(curMinY, cell.edgeStartY)
            (nextMinX, nextMaxX, nextMinY)
          }
        })

      Bounds(
        minX,
        minY,
        maxX - minX,
        (boxMaxY - minY) + (fromInt(4) * externalPadding)
      )

    }
    
  }

  //============================================================================================
  // MUTABLE CELLS
  //

  trait MutableCardinalCell
      extends CardinalCell with SelectableCell { thisCell : CellType with SelectionType => 

    def canExtrude: Boolean
    var isExternal: Boolean

    def selectionAddress = cardinalAddress
    override def address = cardinalAddress.complexAddress

    var cardinalAddress: SCardAddr
    var label: LabelType

  }

  trait MutableNeutralCell
      extends MutableCardinalCell with NeutralCell {
    thisCell : CellType with NeutralCellType with SelectionType =>

    // Selection stuff
    def canExtrude: Boolean = cardinalAddress.boxAddr == Nil
    def canSelect: Boolean = true
    var cardinalAddress: SCardAddr = SCardAddr()

    override def pathString: String = {
      if (canExtrude) {
        var ps : String = "M " ++ edgeStartX.toString ++ " " ++ edgeStartY.toString ++ " "
        ps ++= "V " ++ edgeEndY.toString
        ps
      } else super.pathString
    }

    override def toString: String = 
      label.toString

  }

  trait MutablePolarizedCell
      extends MutableCardinalCell with PolarizedCell {
    thisCell : CellType with PolarizedCellType with SelectionType =>

    var cardinalAddress: SCardAddr = SCardAddr()
    def canExtrude: Boolean = false
    def canSelect: Boolean = false

    // Polarized cells don't need updates on their labels
    def layoutLabel: Unit = ()
    labelNeedsLayout = false

  }

  //============================================================================================
  // MUTABILITY ROUTINES
  //

  def extendPanels(ps: Suite[PanelType]): Suite[PanelType] = {

    val ncn : MTree[STree[SNesting[NeutralCellType]]] = 
      Traverse[MTree].map(ps.head.cardinalNesting)(
        nst => nst.toTreeWith(_ => SDot(createNeutralCell(ps.head.dim + 1, defaultLabel, true)))
      )

    val newPanel = createPanel(ps.head.dim + 1, MFix(ncn), Left(ps.head))

    ps >> newPanel

  }

  def extractSelection: Option[STree[SelectionType]] =
    selectionRoot.flatMap(root => {

      val addr = root.cardinalAddress

      for {
        zp <- seekToCanopy(root.cardinalAddress)
        cut <- zp.focus.takeWhile((n: SNesting[SelectionType]) => n.baseValue.isSelected)
        (et, es) = cut
      } yield et.map(_.baseValue)

    })

  def extrudeAtAddrWithMask[B](tgtVal: LabelType, fillVal: LabelType)(addr: SCardAddr, msk: STree[B]): Option[Unit] = {

    val dim = addr.dim

    val tgtCell = createNeutralCell(dim, tgtVal, false)
    val fillCell = createNeutralCell(dim + 1, fillVal, true)
    
    val extPanels : Suite[PanelType] =
      if (dim == panels.head.dim)
        extendPanels(panels)
      else panels

    val extCardinal : SCardinal[NeutralCellType] =
      Traverse[Suite].map(extPanels)(_.cardinalNesting)
    
    for {
      c <- extCardinal.extrudeWithMask(addr, tgtCell, fillCell)(msk)
    } yield {

      deselectAll

      extPanels.zipWithSuite(c).foreach({
        case (p, n) => p.cardinalNesting = n
      })

      panels = extPanels

      refreshEdges
      refreshAddresses
      
      renderAll

      tgtCell.selectAsRoot

    }

  }

  def extrudeSelectionWith(tgtVal: LabelType, fillVal: LabelType): Option[(SCardAddr, STree[Int])] =
    selectionRoot match {
      case None => None
      case Some(root) => {

        if (root.canExtrude) {

          val tgtCell = createNeutralCell(root.dim, tgtVal, false)
          val fillCell = createNeutralCell(root.dim + 1, fillVal, true)

          val extPanels : Suite[PanelType] =
            if (root.dim == panels.head.dim)
              extendPanels(panels)
            else panels

          val extCardinal : SCardinal[NeutralCellType] =
            Traverse[Suite].map(extPanels)(_.cardinalNesting)

          val extAddr = root.cardinalAddress

          for {
            pr <- extCardinal.extrude(extAddr, tgtCell, fillCell)(_.isSelected)
          } yield {

            val (c, msk) = pr

            deselectAll

            extPanels.zipWithSuite(c).foreach({
              case (p, n) => p.cardinalNesting = n
            })

            panels = extPanels

            refreshEdges
            refreshAddresses
            
            renderAll

            tgtCell.selectAsRoot

            (extAddr, msk)

          }

        } else None

      }
    }

  def extrudeSelection: Unit =
    extrudeSelectionWith(defaultLabel, defaultLabel)

  def loopAtSelectionWith(tgtVal: LabelType, fillVal: LabelType) : Option[SCardAddr] = 
    selectionRoot match {
      case None => None
      case Some(root) => {

        if (root.canExtrude) {

          val tgtCell = createNeutralCell(root.dim + 1, tgtVal, false)
          val fillCell = createNeutralCell(root.dim + 2, fillVal, true)

          val extPanels : Suite[PanelType] =
            if (root.dim == panels.head.dim)
              extendPanels(extendPanels(panels))
            else if (root.dim == panels.head.dim - 1)
              extendPanels(panels)
            else panels

          val extCardinal : SCardinal[NeutralCellType] =
            Traverse[Suite].map(extPanels)(_.cardinalNesting)

          val extAddr = root.cardinalAddress

          for {
            c <- extCardinal.extrudeLoop(extAddr, tgtCell, fillCell)
          } yield {

            deselectAll

            extPanels.zipWithSuite(c).foreach({
              case (p, n) => p.cardinalNesting = n
            })

            panels = extPanels

            refreshEdges
            refreshAddresses

            renderAll
            root.selectAsRoot

            extAddr

          }

        } else None

      }
    }

  def loopAtSelection : Unit = {
    loopAtSelectionWith(defaultLabel, defaultLabel)
  }

  def sproutAtSelectionWith(srcVal: LabelType, fillVal: LabelType): Option[SCardAddr] = 
    selectionRoot match {
      case None => None
      case Some(root) => {
        if (root.isExternal) {

          val srcCell = createNeutralCell(root.dim, srcVal, true)
          val fillCell = createNeutralCell(root.dim + 1, fillVal, true)

          val extPanels : Suite[PanelType] =
            if (root.dim == panels.head.dim)
              extendPanels(panels)
            else panels

          val extCardinal : SCardinal[NeutralCellType] =
            Traverse[Suite].map(extPanels)(_.cardinalNesting)

          val extAddr = root.cardinalAddress

          for {
            c <- extCardinal.sprout(extAddr, srcCell, fillCell)
          } yield {

            deselectAll

            extPanels.zipWithSuite(c).foreach({
              case (p, n) => p.cardinalNesting = n
            })

            panels = extPanels
            root.isExternal = false

            refreshEdges
            refreshAddresses
            renderAll
            srcCell.selectAsRoot

            extAddr

          }

        } else None

      }
    }

  def sproutAtSelection: Unit = 
    sproutAtSelectionWith(defaultLabel, defaultLabel)

}
