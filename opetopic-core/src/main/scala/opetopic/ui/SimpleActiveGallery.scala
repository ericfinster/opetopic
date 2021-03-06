/**
  * SimpleActiveGallery.scala - A Simple Gallery Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scala.collection.mutable.Buffer

import opetopic._

class SimpleActiveGallery[A, F <: ActiveFramework](frmwk: F)(val complex: SComplex[A])(implicit rn: Renderable[A, F])
    extends ActiveStableGallery[F](frmwk) with ComplexGallery[F] {

  import framework._
  import isNumeric._

  type LabelType = A

  type PanelType = SimpleActivePanel
  type CellType = SimpleActiveCell
  type SelectionType = SimpleActiveCell

  type AddressType = (Int, SAddr)

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

  var panels : Suite[PanelType] = buildPanels(complex)
  panels.foreach(p => p.refreshAddresses)

  def setComplex(c: SComplex[A]) = {
    panels = buildPanels(c)
    panels.foreach(p => p.refreshAddresses)
  }

  def createPanel(bn: SNesting[CellType], ed: Either[PanelType, SNesting[CellType]]): PanelType =
    new SimpleActivePanel(bn, ed)

  def createCell(lbl: LabelType, dim: Int, addr: SAddr, isExternal: Boolean): CellType = 
    new SimpleActiveCell(lbl, dim, addr, isExternal)

  def seekToCanopy(addr: AddressType): Option[SZipper[SNesting[SelectionType]]] =
    addr match {
      case (dim, Nil) => Some(SZipper(STree.obj(panels(dim).boxNesting)))
      case (dim, a :: as) => 
        for {
          bz <- panels(dim).boxNesting.seek(as)
          res <- bz.focus match {
            case SDot(_) => None
            case SBox(_, cn) => cn.seekTo(a.dir)
          }
        } yield {
          // res.focus.rootValue.map(n => {
          //   println("found: " + n.baseValue.label.toString)
          // })

          res
        }
    }

  class SimpleActivePanel(
    val boxNesting: SNesting[BoxType],
    val edgeData: Either[PanelType, SNesting[EdgeType]]
  ) extends ActiveStablePanel with ComplexPanel {

    val dim: Int = boxNesting.baseValue.dim

    def refreshAddresses: Unit = 
      boxNesting.foreachWithAddr({
        case (box, addr) => { box.selectionAddress = (dim, addr) }
      })

  }

  class SimpleActiveCell(il: A, val dim: Int, val address: SAddr, val isExternal: Boolean) 
      extends ActiveCell {

    def layoutLabel: Unit = {
      cellRendering = implicitly[Renderable[A, F]].render(framework)(label)
      makeMouseInvisible(labelElement)
    }

    private var myLabel: A = il

    var cellRendering: CellRendering = 
      implicitly[Renderable[A, F]].
        render(framework)(label)
    
    def label: A = myLabel
    def label_=(l: A): Unit = {
      myLabel = l
      labelNeedsLayout = true
    }

    val canSelect: Boolean = true
    var selectionAddress: AddressType = (0, Nil)

    override def toString: String =
      label.toString

    makeMouseInvisible(labelElement)
    
  }

}
