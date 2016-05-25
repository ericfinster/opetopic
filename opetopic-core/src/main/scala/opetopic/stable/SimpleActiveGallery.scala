/**
  * SimpleActiveGallery.scala - A Simple Gallery Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scala.collection.mutable.Buffer

import opetopic._
import opetopic.ui._

class SimpleActiveGallery[A : Renderable, F <: ActiveFramework](frmwk: F)(val complex: SComplex[A]) 
    extends ActiveStableGallery[A, F](frmwk) {

  import framework._
  import isNumeric._

  type BoxType = SimpleActiveCell
  type EdgeType = SimpleActiveCell

  type PanelType = SimpleActivePanel

  var onCellClick: ActiveBox => Unit = { _ => () }

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

  val panels : Suite[PanelType] = buildPanels(complex)

  class SimpleActivePanel(
    val boxNesting: SNesting[BoxType],
    val edgeNesting: SNesting[EdgeType]
  ) extends ActiveStablePanel {
    def dim: Int = boxNesting.baseValue.dim
  }

  class SimpleActiveCell(val label: A, val dim: Int, val address: SAddr, val isExternal: Boolean) 
      extends ActiveBox 
      with ActiveEdge {

    val labelBE: BoundedElement =
      implicitly[Renderable[A]].
        render(framework)(label)

    makeMouseInvisible(labelElement)

    def labelBounds: Bounds = labelBE.bounds
    def labelElement: Element = labelBE.element

    def onClick: Unit = onCellClick(this)

    def onMouseOver: Unit = {
      boxRect.stroke = "red"
      edgePath.stroke = "red"
    }

    def onMouseOut: Unit = {
      boxRect.stroke = "black"
      edgePath.stroke = "black"
    }

  }

  //
  //  Gallery Construction
  //

  // This is seriously ugly and should be redone in a more
  // systematic way ...

  def buildCellNesting(dim: Int, n: SNesting[A]): SNesting[SimpleActiveCell] =
    n.foldNestingWithAddr[SNesting[SimpleActiveCell]]()({
      case (a, addr) => SDot(new SimpleActiveCell(a, dim, addr, true))
    })({
      case (a, addr, cn) => SBox(new SimpleActiveCell(a, dim, addr, false), cn)
    })

  def buildPanels(c: SComplex[A]) : Suite[PanelType] =
    c match {
      case ||(n) => {

        val objNesting = buildCellNesting(0, n)

        val inEdge = new SimpleActiveCell(n.baseValue, -1, Nil, true)
        val outEdge = new SimpleActiveCell(n.baseValue, -1, Nil, false)

        val panel = new SimpleActivePanel(
          objNesting,
          SBox(outEdge, STree.obj(SDot(inEdge)))
        )

        objNesting.foldNesting(c => { c.outgoingEdge = Some(outEdge) })((_, _) => ())

        ||(panel)

      }
      case tl >> hd => {

        val newTail = buildPanels(tl)

        val prevCellNesting = newTail.head.boxNesting
        val thisCellNesting = buildCellNesting(prevCellNesting.baseValue.dim + 1, hd)

        // Connect the edges
        thisCellNesting match {
          case SDot(c) => c.outgoingEdge = Some(prevCellNesting.baseValue)
          case SBox(_, cn) => 
            for {
              sp <- cn.spine
              _ <- sp.matchTraverse[EdgeType, Unit](prevCellNesting.toTree)({
                case (c, e) => Some({ c.outgoingEdge = Some(e) })
              })
            } yield ()
        }

        val panel = new SimpleActivePanel(
          thisCellNesting,
          prevCellNesting
        )

        newTail >> panel

      }
    }


}
