/**
  * StableGallery.scala - A Gallery for Stable Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

abstract class StableGallery[F <: UIFramework](final val framework: F) 
    extends LayoutContext[F] { thisGallery =>

  import framework._
  import isNumeric._

  type LabelType

  type CellType <: GalleryCell
  type BoxType = CellType
  type EdgeType = CellType

  type PanelType <: StablePanel

  def panels: Suite[PanelType]
  def element: Element

  def boxComplex: SComplex[BoxType] = 
    panels.map(_.boxNesting)

  //
  //  Gallery Options
  //

  def width: Size
  def height: Size
  def minViewX: Option[Size]
  def minViewY: Option[Size]
  def spacing: Size
  def manageViewport : Boolean

  def firstPanel: Option[Int]
  def lastPanel: Option[Int]

  //
  //  Gallery Layout
  //

  def panelElementsAndBounds: (Bounds, Suite[Element]) = {

    val pStart = firstPanel.map(i => {
      if (i > 0) i - 1 else i
    }).getOrElse(0)

    val pEnd = lastPanel.getOrElse(panels.length)

    val ps = 
      if (pStart > 0)
        Suite.fromList(panels.take(pEnd).splitAt(pStart)._2.reverse).getOrElse(panels)
      else 
        panels.take(pEnd)

    val (maxHeight, pbnds): (Size, Suite[(PanelType, Bounds)]) = 
      ps.mapAccumL[Size, (PanelType, Bounds)](zero)({
        case (h, p) => {

          val lr = 
            for {
              bnds <- p.layout
            } yield (isOrdered.max(h, bnds.height), (p, bnds))


          lr.getOrElse((h, (p, Bounds())))

        }
      })

    val (xPos, els): (Size, Suite[Element]) = 
      pbnds.mapAccumL[Size, Element](spacing)({
        case (xPos, (p, bnds)) => {

          val xTrans = (-bnds.x) + xPos
          val yTrans = (-bnds.y) - bnds.height - half(maxHeight - bnds.height)

          val tEl = translate(p.element, xTrans, yTrans)

          (xPos + bnds.width + spacing, tEl)

        }
      })

    (Bounds(zero, -maxHeight, xPos, maxHeight), els)

  }

  //============================================================================================
  // CELLS
  //

  abstract class GalleryCell extends CellBox with CellEdge { thisBox : BoxType => 

    def dim: Int
    def address: SAddr

    def label: LabelType

    def face: Option[SComplex[LabelType]] = 
      boxComplex.face(dim)(address).map(_.map(_.label))

    def boxFace: Option[SComplex[BoxType]] =
      boxComplex.face(dim)(address)

    def cellRendering: CellRendering

    def labelBounds: Bounds = cellRendering.boundedElement.bounds
    def labelElement: Element = cellRendering.boundedElement.element

    def colorSpec: ColorSpec = 
      cellRendering.colorSpec

  }

  //============================================================================================
  // PANELS
  //

  abstract class StablePanel {

    def boxNesting: SNesting[BoxType]
    def edgeNesting: SNesting[EdgeType]

    def dim: Int

    def element: Element

    //
    //  Edge Layout Tree
    //

    def edgeLayoutTree(en: SNesting[EdgeType]): Option[STree[LayoutMarker]] = 
      for {
        sp <- en.spine(SDeriv(SLeaf))  // Default is an object
      } yield sp.map(edge => {
        edge.clearEdge
        val edgeMarker = new EdgeStartMarker(edge)
        LayoutMarker(
          edgeMarker, edgeMarker, true,
          leftInternalMargin = halfLeafWidth,
          rightInternalMargin = halfLeafWidth
        )
      })

    //
    //  Panel Bounds Calculation
    //

    def bounds: Bounds = {

      val baseBox = boxNesting.baseValue

      val (panelX, panelWidth) =
        boxNesting match {
          case SBox(bx, _) => (baseBox.x, baseBox.width)
          case SDot(bx) => {
            edgeNesting match {
              case SDot(_) => (baseBox.x, baseBox.width) // Error?
              case SBox(_, srcs) => {

                val (minX, maxX) = (srcs.map(_.baseValue).toList foldLeft (baseBox.x, baseBox.x + baseBox.width))({
                  case ((curMin, curMax), edge) =>
                    (isOrdered.min(curMin, edge.edgeStartX), 
                      isOrdered.max(curMax, edge.edgeStartX))
                })

                (minX, maxX - minX)

              }
            }
          }
        }

      Bounds(
        panelX,
        baseBox.y - (fromInt(2) * externalPadding),
        panelWidth,
        baseBox.height + (fromInt(4) * externalPadding)
      )

    }

    //
    //  Panel Layout
    //

    def layout: Option[Bounds] = {

      for {
        lvs <- edgeLayoutTree(edgeNesting)
        baseLayout <- thisGallery.layout(boxNesting, lvs)
      } yield {

        // println("Finished layout in dimension: " + dim)

        val baseBox = boxNesting.baseValue

        for { l <- lvs } {
          l.rootEdge.rootY = baseBox.y - (fromInt(2) * externalPadding)
        }

        // Set the position of the outgoing edge
        baseLayout.rootEdge.endMarker.rootY =
          baseBox.rootY + (fromInt(2) * externalPadding)

        bounds

      }
    }

  }

}
