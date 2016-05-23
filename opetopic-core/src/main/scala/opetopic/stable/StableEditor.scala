/**
  * StableEditor.scala - A Stable Cardinal Editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Traverse
import scalaz.syntax.traverse._

import opetopic._
import opetopic.ui._

class StableEditor[A : Renderable, F <: ActiveFramework](frmwk: F)(c: SCardinal[Option[A]])
    extends ActiveStableGallery[Polarity[Option[A]], F](frmwk) {

  import framework._
  import isNumeric._

  type BoxType = EditorCell
  type EdgeType = EditorCell

  type PanelType = EditorPanel

  type OptA = Option[A]
  type PolOptA = Polarity[Option[A]]

  val renderer : Renderable[PolOptA] = 
    Renderable[PolOptA]

  //
  //  Visual Options
  //

  var internalPadding : Size = fromInt(400)
  var externalPadding : Size = fromInt(600)
  var leafWidth : Size = fromInt(200)
  var strokeWidth : Size = fromInt(100)
  var cornerRadius : Size = fromInt(200)

  //
  //  Gallery Options
  //

  var width: Size = fromInt(900)
  var height: Size = fromInt(300)
  var minViewX: Option[Size] = None
  var minViewY: Option[Size] = None
  var spacing: Size = fromInt(2000)
  var manageViewport : Boolean = false

  //============================================================================================
  // EDITOR DATA
  //

  var panels : Suite[PanelType] = buildPanels(c)._1

  def cardinal: SCardinal[NeutralCell] = 
    Traverse[Suite].map(panels)(_.cardinalNesting)

  def complex: SComplex[EditorCell] = 
    Traverse[Suite].map(panels)(_.boxNesting)

  //============================================================================================
  // INITIALIZATION
  //

  def buildPanels(c: SCardinal[OptA]): (Suite[PanelType], Int) = 
    c match {
      case ||(cn) => {
        val bn = Traverse[MTree].map(cn)(buildCells(0, _))

        val inEdge = new NeutralCell(-1, None, true)
        val outEdge = new NeutralCell(-1, None, false)
        val en = SBox(outEdge, STree.obj(SDot(inEdge)))

        (||(new EditorPanel(0, bn, Right(en))), 0)
      }
      case tl >> hd => {
        val (pt, d) = buildPanels(tl)
        val bn = Traverse[MTree].map(hd)(buildCells(d + 1, _))
        (pt >> new EditorPanel(d + 1, bn, Left(pt.head)), d + 1)
      }
    }

  // Here we should take an address prefix as well and store
  // the actual cardinal address...
  def buildCells(d: Int, n: SNesting[OptA]): SNesting[NeutralCell] = 
    n.foldNestingWithAddr[SNesting[NeutralCell]]()({
      case (optA, addr) => SDot(new NeutralCell(d, optA, true))
    })({
      case (optA, addr, cn) => SBox(new NeutralCell(d, optA, false), cn)
    })

  //============================================================================================
  // REFRESH ROUTINES
  //

  override def renderAll: Unit = {
    refreshEdges
    println("Edge refresh complete, rendering ...")
    super.renderAll
  }

  def refreshEdges: Unit = 
    panels.foreach((p: EditorPanel) => p.refreshEdges)

  //============================================================================================
  // PANEL IMPLEMENTATION
  //

  class EditorPanel(
    val dim: Int,
    var cardinalNesting: SCardNst[NeutralCell],
    val edgeData: Either[EditorPanel, SNesting[EditorCell]]
  ) extends ActiveStablePanel {

    val positiveCell: PositiveCell = new PositiveCell(dim)
    val negativeCell: NegativeCell = new NegativeCell(dim)

    def boxNesting: SNesting[EditorCell] = 
      cardinalNesting.toNesting(
        positiveCell, 
        negativeCell
      )

    // Gotta take care of the object case ...
    def edgeNesting: SNesting[EditorCell] = 
      edgeData match {
        case Left(pp) => pp.boxNesting
        case Right(en) => en
      }

    // This should be made into a gallery routine ...
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

  }

  //============================================================================================
  // CELL IMPLEMENTATIONS
  //

  abstract class EditorCell extends ActiveBox with ActiveEdge {

    def onClick: Unit = ()
    def onMouseOver: Unit = ()
    def onMouseOut: Unit = ()

  }

  class NeutralCell(
    val dim: Int,
    var optA: Option[A],
    var isExternal: Boolean
  ) extends EditorCell {

    // FIXME!!
    def address: SAddr = Nil

    // Label data
    def label: PolOptA = Neutral(optA)
    var labelBE: BoundedElement = renderer.render(framework)(label)
    def labelElement: Element = labelBE.element
    def labelBounds: Bounds = labelBE.bounds

  }

  abstract class PolarizedCell extends EditorCell {
    val address: SAddr = Nil
    def polarityElement: BoundedElement
    def labelElement = polarityElement.element
    def labelBounds = polarityElement.bounds
  }

  class NegativeCell(val dim: Int) extends PolarizedCell {

    val label: PolOptA = Negative()
    val isExternal: Boolean = true

    val polarityElement: BoundedElement = 
      renderer.render(framework)(label)

    boxRect.fill = "lightgrey"

  }

  class PositiveCell(val dim: Int) extends PolarizedCell {

    val label: PolOptA = Positive()
    val isExternal: Boolean = false

    val polarityElement: BoundedElement = 
      renderer.render(framework)(label)

    boxRect.fill = "lightgrey"

  }

}

object StableEditor {

  // def apply[A, F <: ActiveFramework](f: F)(implicit r: Renderable[A]): StableEditor[A, F] = 
  //   new StableEditor(f)(SCardinal(None))

  def apply[A, F <: ActiveFramework](f: F)(c: SComplex[A])(implicit r: Renderable[A]): StableEditor[A, F] = 
    new StableEditor(f)(SCardinal(c.map(Some(_))))

  // def apply[A, F <: ActiveFramework](f: F)(c: SComplex[Option[A]])(implicit r: Renderable[A]): StableEditor[A, F] = 
  //   new StableEditor(f)(SCardinal(c))

}
