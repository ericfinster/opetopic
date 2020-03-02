/**
  * CardinalEditor.scala - A Cardinal Cardinal Editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

// The StableEditor builds in the option type.
// But this seems clunky and undesirable.
// Here is a version without that assumption.

// trait Pointed[A] {
//   def pt: A
// }

// class CardinalEditor[A, F <: ActiveFramework](frmwk: F)(c: SCardinal[A])(
//   implicit
//     r: Renderable[A],
//     p: Pointed[A]
// ) extends ActiveStableGallery[F](frmwk)  {

//   import framework._
//   import isNumeric._

//   type LabelType = A

//   type PanelType = EditorPanel
//   type CellType = EditorCell

//   type AddressType = SCardAddr
//   type SelectionType = EditorCell

//   type PolA = Polarity[A]

//   val renderer : Renderable[PolA] = 
//     Renderable[PolA]

//   //
//   //  Visual Options
//   //

//   var internalPadding : Size = fromInt(400)
//   var externalPadding : Size = fromInt(600)
//   var decorationPadding : Size = fromInt(300)
//   var leafWidth : Size = fromInt(200)
//   var strokeWidth : Size = fromInt(100)
//   var cornerRadius : Size = fromInt(200)

//   //
//   //  Gallery Options
//   //

//   var width: Size = fromInt(900)
//   var height: Size = fromInt(300)
//   var panelSpacing: Size = fromInt(2000)

//   var layoutWidth: Bounds => Size = 
//     (pb: Bounds) => width

//   var layoutHeight: Bounds => Size = 
//     (pb: Bounds) => height

//   var layoutViewport: Bounds => Bounds = 
//     (pb: Bounds) => pb

//   var firstPanel: Option[Int] = None
//   var lastPanel: Option[Int] = None

//   //============================================================================================
//   // EDITOR DATA
//   //

//   var panels : Suite[PanelType] = buildPanels(c)._1

//   def cardinal: SCardinal[NeutralCell] =
//     Traverse[Suite].map(panels)(_.cardinalNesting)

//   def cardinal_=(c: SCardinal[A]): Unit = {
//     panels = buildPanels(c)._1
//   }

//   def complex: SComplex[EditorCell] = 
//     Traverse[Suite].map(panels)(_.boxNesting)

//   //============================================================================================
//   // INITIALIZATION
//   //

//   def buildPanels(c: SCardinal[A]): (Suite[PanelType], Int) = 
//     c match {
//       case ||(cn) => {
//         val bn = Traverse[MTree].map(cn)(buildCells(0, _))

//         val inEdge = new NeutralCell(-1, p.pt, true)
//         val outEdge = new NeutralCell(-1, p.pt, false)
//         val en = SBox(outEdge, STree.obj(SDot(inEdge)))

//         (||(new EditorPanel(0, bn, Right(en))), 0)
//       }
//       case tl >> hd => {
//         val (pt, d) = buildPanels(tl)
//         val bn = Traverse[MTree].map(hd)(buildCells(d + 1, _))
//         (pt >> new EditorPanel(d + 1, bn, Left(pt.head)), d + 1)
//       }
//     }

//   // Here we should take an address prefix as well and store
//   // the actual cardinal address...
//   def buildCells(d: Int, n: SNesting[A]): SNesting[NeutralCell] = 
//     n.foldNestingWithAddr[SNesting[NeutralCell]]()({
//       case (a, addr) => SDot(new NeutralCell(d, a, true))
//     })({
//       case (a, addr, cn) => SBox(new NeutralCell(d, a, false), cn)
//     })

//   //============================================================================================
//   // SELECTION SEEKING
//   //

//   def seekToAddress(addr: SCardAddr): Option[SNstZipper[NeutralCell]] = 
//     cardinal.seekNesting(addr)

//   def seekToCanopy(addr: SCardAddr): Option[SZipper[SNesting[NeutralCell]]] = 
//     cardinal.seekCanopy(addr)

//   //============================================================================================
//   // REFRESH ROUTINES
//   //

//   override def renderAll: Unit = {
//     refreshEdges
//     refreshAddresses
//     super.renderAll
//   }

//   // These should be combined somehow ...
//   def refreshEdges: Unit = 
//     panels.foreach(_.refreshEdges)

//   def refreshAddresses: Unit = {
//     panels.foreach(_.refreshAddresses)
//   }

//   //============================================================================================
//   // MUTABILITY ROUTINES
//   //

//   def extendPanels(ps: Suite[PanelType]): Suite[PanelType] = {

//     val ncn : MTree[STree[SNesting[NeutralCell]]] = 
//       Traverse[MTree].map(ps.head.cardinalNesting)(
//         nst => nst.toTreeWith(_ => SDot(new NeutralCell(ps.head.dim + 1, p.pt, true)))
//       )

//     val newPanel = new EditorPanel(ps.head.dim + 1, MFix(ncn), Left(ps.head))

//     ps >> newPanel

//   }

//   def extractSelection: Option[STree[NeutralCell]] =
//     selectionRoot.flatMap(root => {

//       val addr = root.cardinalAddress

//       for {
//         zp <- seekToCanopy(root.cardinalAddress)
//         cut <- zp.focus.takeWhile((n: SNesting[NeutralCell]) => n.baseValue.isSelected)
//         (et, es) = cut
//       } yield et.map(_.baseValue)

//     })

//   def extrudeSelectionWith(tgtVal: A, fillVal: A): Option[(SCardAddr, STree[Int])] =
//     selectionRoot match {
//       case None => None
//       case Some(root) => {

//         if (root.canExtrude) {

//           val tgtCell = new NeutralCell(root.dim, tgtVal, false)
//           val fillCell = new NeutralCell(root.dim + 1, fillVal, true)

//           val extPanels : Suite[PanelType] =
//             if (root.dim == panels.head.dim)
//               extendPanels(panels)
//             else panels

//           val extCardinal : SCardinal[NeutralCell] =
//             Traverse[Suite].map(extPanels)(_.cardinalNesting)

//           val extAddr = root.cardinalAddress

//           for {
//             pr <- extCardinal.extrude(extAddr, tgtCell, fillCell)(_.isSelected)
//           } yield {

//             val (c, msk) = pr

//             deselectAll

//             extPanels.zipWithSuite(c).foreach({
//               case (p, n) => p.cardinalNesting = n
//             })

//             panels = extPanels

//             renderAll
//             tgtCell.selectAsRoot

//             (extAddr, msk)

//           }

//         } else None

//       }
//     }

//   def extrudeSelection: Unit =
//     extrudeSelectionWith(p.pt, p.pt)

//   def loopAtSelectionWith(tgtVal: A, fillVal: A) : Option[SCardAddr] = 
//     selectionRoot match {
//       case None => None
//       case Some(root) => {

//         if (root.canExtrude) {

//           val tgtCell = new NeutralCell(root.dim + 1, tgtVal, false)
//           val fillCell = new NeutralCell(root.dim + 2, fillVal, true)

//           val extPanels : Suite[PanelType] =
//             if (root.dim == panels.head.dim)
//               extendPanels(extendPanels(panels))
//             else if (root.dim == panels.head.dim - 1)
//               extendPanels(panels)
//             else panels

//           val extCardinal : SCardinal[NeutralCell] =
//             Traverse[Suite].map(extPanels)(_.cardinalNesting)

//           val extAddr = root.cardinalAddress

//           for {
//             c <- extCardinal.extrudeLoop(extAddr, tgtCell, fillCell)
//           } yield {

//             deselectAll

//             extPanels.zipWithSuite(c).foreach({
//               case (p, n) => p.cardinalNesting = n
//             })

//             panels = extPanels

//             renderAll
//             root.selectAsRoot

//             extAddr

//           }

//         } else None

//       }
//     }

//   def loopAtSelection : Unit = {
//     loopAtSelectionWith(p.pt, p.pt)
//   }

//   def sproutAtSelectionWith(srcVal: A, fillVal: A): Option[SCardAddr] = 
//     selectionRoot match {
//       case None => None
//       case Some(root) => {
//         if (root.isExternal) {

//           val srcCell = new NeutralCell(root.dim, srcVal, true)
//           val fillCell = new NeutralCell(root.dim + 1, fillVal, true)

//           val extPanels : Suite[PanelType] =
//             if (root.dim == panels.head.dim)
//               extendPanels(panels)
//             else panels

//           val extCardinal : SCardinal[NeutralCell] =
//             Traverse[Suite].map(extPanels)(_.cardinalNesting)

//           val extAddr = root.cardinalAddress

//           for {
//             c <- extCardinal.sprout(extAddr, srcCell, fillCell)
//           } yield {

//             deselectAll

//             extPanels.zipWithSuite(c).foreach({
//               case (p, n) => p.cardinalNesting = n
//             })

//             panels = extPanels
//             root.isExternal = false

//             renderAll
//             srcCell.selectAsRoot

//             extAddr

//           }

//         } else None

//       }
//     }

//   def sproutAtSelection: Unit = {
//     sproutAtSelectionWith(p.pt, p.pt)
//   }

//   //============================================================================================
//   // PANEL IMPLEMENTATION
//   //

//   class EditorPanel(
//     val dim: Int,
//     var cardinalNesting: SCardNst[NeutralCell],
//     val edgeData: Either[EditorPanel, SNesting[EditorCell]]
//   ) extends ActiveStablePanel {

//     val positiveCell: PositiveCell = new PositiveCell(dim)
//     val negativeCell: NegativeCell = new NegativeCell(dim)

//     def boxNesting: SNesting[EditorCell] = 
//       cardinalNesting.toNesting(
//         positiveCell, 
//         negativeCell
//       )

//     def edgeNesting: SNesting[EditorCell] = 
//       edgeData match {
//         case Left(pp) => pp.boxNesting
//         case Right(en) => en
//       }

//     def refreshAddresses: Unit = 
//       cardinalNesting.foreachWithAddr(
//         (box, addr) => { box.cardinalAddress = addr }
//       )

//     def refreshEdges: Unit = 
//       edgeData match {
//         case Left(pp) => {

//           boxNesting match {
//             case SDot(c) => c.outgoingEdge = Some(pp.boxNesting.baseValue)
//             case SBox(_, cn) => 
//               for {
//                 sp <- cn.spine
//                 _ <- sp.matchTraverse[EdgeType, Unit](pp.boxNesting.toTree)({
//                   case (c, e) => Some({ c.outgoingEdge = Some(e) })
//                 })
//               } { }
//           }
          
//         }
//         case Right(en) => {
//           boxNesting.map(c => c.outgoingEdge = Some(en.baseValue))
//         }
//       }

//   }

//   //============================================================================================
//   // CELL IMPLEMENTATIONS
//   //

//   abstract class EditorCell extends ActiveCell {
//     def canExtrude: Boolean
//     def cardinalAddress: SCardAddr
//     def selectionAddress = cardinalAddress
//     def address = cardinalAddress.complexAddress
//     var isExternal: Boolean
//     var label: A
//   }

//   class NeutralCell(
//     val dim: Int,
//     initLabel: A, 
//     var isExternal: Boolean
//   ) extends EditorCell {

//     var cardinalAddress: SCardAddr = SCardAddr()

//     private var myLabel: A = initLabel

//     def label: A = myLabel
//     def label_=(a: A): Unit = {
//       myLabel = a
//       cellRendering = renderer.render(framework)(Neutral(a))
//     }

//     var cellRendering: CellRendering = 
//       renderer.render(framework)(Neutral(label))

//     // Selection stuff
//     def canExtrude: Boolean = cardinalAddress.boxAddr == Nil
//     def canSelect: Boolean = true

//     override def toString: String = 
//       label.toString

//   }

//   abstract class PolarizedCell extends EditorCell {
//     val cardinalAddress: SCardAddr = SCardAddr()
//     def canExtrude = false
//     def canSelect = false
//     override def onClick: Unit = deselectAll
//     override def onMouseOver: Unit = ()
//     override def onMouseOut: Unit = ()
//     override def onHover: Unit = ()
//     override def onUnhover: Unit = ()

//   }

//   class NegativeCell(val dim: Int) extends PolarizedCell {

//     var label: A = p.pt
//     var isExternal: Boolean = true

//     val cellRendering: CellRendering = 
//       renderer.render(framework)(Negative())

//     override def toString = "-"

//   }

//   class PositiveCell(val dim: Int) extends PolarizedCell {

//     var label: A = p.pt
//     var isExternal: Boolean = false

//     val cellRendering: CellRendering = 
//       renderer.render(framework)(Positive())

//     override def toString = "+"

//   }

// }

// object CardinalEditor {

//   // def apply[A, F <: ActiveFramework](f: F)(c: SComplex[A])(
//   //   implicit
//   //     r: Renderable[A],
//   //     d: Pointed[A]
//   // ): CardinalEditor[A, F] =
//   //   new CardinalEditor(f)(SCardinal(c))

//   def apply[A, F <: ActiveFramework](f: F)(
//     implicit
//       r: Renderable[A],
//       p: Pointed[A]
//   ): CardinalEditor[A, F] =
//     new CardinalEditor(f)(SCardinal(p.pt))
  
// }
