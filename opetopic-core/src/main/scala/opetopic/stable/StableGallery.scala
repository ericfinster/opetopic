/**
  * StableGallery.scala - A Gallery for Stable Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Traverse
import scalaz.syntax.traverse._
import scalaz.std.option._

import opetopic.ui._

abstract class StableGallery[A, F <: UIFramework](final val framework: F) 
    extends LayoutContext[A, F] { thisGallery =>

  import framework._
  import isNumeric._

  type PanelType <: StablePanel

  def panels: Suite[PanelType]
  def element: Element

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

  def layout: Option[Bounds] = {

    for {
      bnds <- panels.traverse(_.layout)
    } yield {

      // We are going to shift and locate the panels here

      val maxHeight : Size = bnds.map(_.height).foldRight(fromInt(0))(isOrdered.max(_, _))
      var xPos : Size = spacing

      for {
        p <- panels
      } {

        // Annoying that we recalculate this here.  It's just a typing issue ...
        val b = p.bounds
        val xTrans = (-b.x) + xPos
        val yTrans = (-b.y) - b.height - half(maxHeight - b.height)

        translate(p.element, xTrans, yTrans)

        xPos = xPos + b.width + spacing

      }

      val (viewY, viewHeight) =
        minViewY match {
          case None => (-maxHeight, maxHeight)
          case Some(mvh) =>
            if (mvh < maxHeight) {
              (-maxHeight, maxHeight)
            } else {
              val offset = half(mvh - maxHeight)
              (-maxHeight - offset, mvh)
            }
        }

      val (viewX, viewWidth) =
        minViewX match {
          case None => (zero, xPos)
          case Some(mvw) =>
            if (mvw < xPos) {
              (zero, xPos)
            } else {
              val offset = half(mvw - xPos)
              (-offset, mvw)
            }
        }

      Bounds(zero, viewY, xPos, viewHeight)

    }
  }

  //============================================================================================
  // PANELS
  //

  abstract class StablePanel {

    def boxNesting: SNesting[BoxType]
    def edgeNesting: SNesting[EdgeType]

    def element: Element

    //
    //  Edge Layout Tree
    //

    def edgeLayoutTree(en: SNesting[EdgeType]): Option[STree[LayoutMarker]] = 
      for {
        sp <- en.spine(SDeriv(SNode(SLeaf, SLeaf)))  // Default is an object
      } yield sp.map(edge => {
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
      println("Laying out panel")
      for {
        lvs <- edgeLayoutTree(edgeNesting)
        _ = println("Got the layou tree")
        baseLayout <- thisGallery.layout(boxNesting, lvs)
        _ = println("Layout succeeded")
      } yield {

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
