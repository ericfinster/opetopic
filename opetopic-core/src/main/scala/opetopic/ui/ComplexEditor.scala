/**
  * ComplexEditor.scala - A Stable Complex Editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

// package opetopic.ui

// import opetopic._
// import mtl._

// class ComplexEditor[A : Renderable, F <: ActiveFramework](frmwk: F)(c: SComplex[Option[A]])
//     extends ActiveStableGallery[F](frmwk) with ComplexGallery[F] {

//   import framework._
//   import isNumeric._

//   type LabelType = Option[A]

//   type PanelType = EditorPanel
//   type CellType = EditorCell

//   type AddressType = FaceAddr
//   type SelectionType = EditorCell

//   type OptA = Option[A]

//   val renderer : Renderable[OptA] = 
//     Renderable[OptA]

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
//   // CREATION ROUTINES
//   //

//   def createPanel(bn: SNesting[CellType], ed: Either[PanelType, SNesting[CellType]]): PanelType =
//     new EditorPanel(bn, ed)

//   def createCell(lbl: LabelType, dim: Int, addr: SAddr, isExternal: Boolean): CellType = 
//     new EditorCell(lbl, dim, addr, isExternal)

//   //============================================================================================
//   // EDITOR DATA
//   //

//   var panels : Suite[PanelType] = buildPanels(c)

//   def complex: SComplex[EditorCell] = 
//     Traverse[Suite].map(panels)(_.boxNesting)

//   //============================================================================================
//   // SELECTION SEEKING
//   //

//   def seekToCanopy(fa: FaceAddr): Option[SZipper[SNesting[SelectionType]]] = {

//     // val dim = complex.dim - fa.codim
//     val nst = panels(complex.dim - fa.codim).boxNesting

//     fa.address match {
//       case Nil => Some(SZipper(STree.obj(nst)))
//       case d :: ds =>
//         for {
//           bz <- nst.seek(ds)
//           res <- bz.focus match {
//             case SDot(_) => None
//             case SBox(_, cn) => cn.seekTo(d.dir)
//           }
//         } yield res
//     }

//   }

//   //============================================================================================
//   // PANEL IMPLEMENTATION
//   //


//   class EditorPanel(
//     val boxNesting: SNesting[BoxType],
//     val edgeData: Either[PanelType, SNesting[EdgeType]]
//   ) extends ActiveStablePanel with ComplexPanel {

//     val dim: Int = boxNesting.baseValue.dim
//     val codim = complex.dim - dim 

//     def refreshAddresses: Unit = 
//       boxNesting.foreachWithAddr({
//         case (box, addr) => { box.selectionAddress = FaceAddr(codim, addr) }
//       })

//   }

//   class EditorCell(val dim: Int, val address: SAddr, val isExternal: Boolean) 
//       extends ActiveCell {

//     val codim = complex.dim - dim

//     def this(il: OptA, dim: Int, address: SAddr, isExternal: Boolean) = {
//       this(dim, address, isExternal)
//       label = il
//     }

//     private var myLabel: OptA = None

//     def label: OptA = myLabel
//     def label_=(l: OptA): Unit = {
//       myLabel = l
//       cellRendering = implicitly[Renderable[OptA]].render(framework)(label)
//     }

//     var cellRendering: CellRendering = 
//       implicitly[Renderable[OptA]].
//         render(framework)(label)

//     val canSelect: Boolean = true
//     var selectionAddress: AddressType = FaceAddr(codim, address) 

//   }


// }
