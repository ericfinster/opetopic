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

  type BoxType <: GalleryBox
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

  //
  //  Gallery Layout
  //

  def panelElementsAndBounds: (Bounds, Suite[Element]) = {

    // I see, we have to traverse twice since we need to know
    // the maximum height in order to shift everything to the
    // center.

    val (maxHeight, pbnds): (Size, Suite[(PanelType, Bounds)]) = 
      panels.mapAccumL[Size, (PanelType, Bounds)](zero)({
        case (h, p) => {

          val lr = 
            for {
              bnds <- p.layout
            } yield (isOrdered.max(h, bnds.height), (p, bnds))


          lr.getOrElse((h, (p, Bounds())))

        }
      })


    // Now the idea is that we do it again, this time with the 
    // parameter being the x-shift, and the result being the 
    // elements shifted into place

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

  // // Get rid of this.  The previous implementation is more
  // // general and more useful.
  // def layout: Option[Bounds] = {

  //   for {
  //     bnds <- panels.traverse(_.layout)
  //   } yield {

  //     // We are going to shift and locate the panels here

  //     val maxHeight : Size = bnds.map(_.height).foldRight(fromInt(0))(isOrdered.max(_, _))
  //     var xPos : Size = spacing

  //     for {
  //       p <- panels
  //     } {

  //       // Annoying that we recalculate this here.  It's just a typing issue ...
  //       val b = p.bounds
  //       val xTrans = (-b.x) + xPos
  //       val yTrans = (-b.y) - b.height - half(maxHeight - b.height)

  //       translate(p.element, xTrans, yTrans)

  //       xPos = xPos + b.width + spacing

  //     }

  //     val (viewY, viewHeight) =
  //       minViewY match {
  //         case None => (-maxHeight, maxHeight)
  //         case Some(mvh) =>
  //           if (mvh < maxHeight) {
  //             (-maxHeight, maxHeight)
  //           } else {
  //             val offset = half(mvh - maxHeight)
  //             (-maxHeight - offset, mvh)
  //           }
  //       }

  //     val (viewX, viewWidth) =
  //       minViewX match {
  //         case None => (zero, xPos)
  //         case Some(mvw) =>
  //           if (mvw < xPos) {
  //             (zero, xPos)
  //           } else {
  //             val offset = half(mvw - xPos)
  //             (-offset, mvw)
  //           }
  //       }

  //     Bounds(zero, viewY, xPos, viewHeight)

  //   }
  // }

  //============================================================================================
  // CELLS
  //

  trait GalleryBox extends CellBox { thisBox : BoxType => 

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
        sp <- en.spine(SDeriv(SNode(SLeaf, SLeaf)))  // Default is an object
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
