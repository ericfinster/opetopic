/**
  * StableGallery.scala - A stable opetopic gallery
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Traverse
import scalaz.syntax.traverse._
import scalaz.std.option._
import scalaz.std.list._

import opetopic.ui._

abstract class StableGallery[A, F <: UIFramework](final val framework: F) extends VisualComplex[A, F] { thisGallery =>

  import framework._
  import isNumeric._

  type PanelType <: StablePanel

  def panels: List[PanelType]

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

    def baseCell: CellType
    def element: Element

    //
    //  Panel Bounds Calculation
    //

    def bounds: Bounds = {
      
      val (panelX, panelWidth) =
        baseCell.canopy match {
          case Some(_) => (baseCell.x, baseCell.width)
          case None => {
            baseCell.sourceTree match {
              case None => (baseCell.x, baseCell.width)
              case Some(srcs) => {

                val (minX, maxX) = (srcs.toList foldLeft (baseCell.x, baseCell.x + baseCell.width))({
                  case ((curMin, curMax), edge) =>
                    (isOrdered.min(curMin, edge.edgeStartX), isOrdered.max(curMax, edge.edgeStartX))
                })

                (minX, maxX - minX)

              }
            }
          }
        }

      Bounds(
        panelX,
        baseCell.y - (fromInt(2) * externalPadding),
        panelWidth,
        baseCell.height + (fromInt(4) * externalPadding)
      )

    }

    //
    //  Panel Layout
    //

    def layout: Option[Bounds] = {

      if (baseCell.target == None) {

        // No edges.  Assume this is an object.

        val thisMarker = new LayoutMarker(DummyMarker(), DummyMarker(), true)

        for {
          baseLayout <- baseCell.layout(SNode(thisMarker, SNode(SLeaf, SLeaf)))
        } yield bounds

      } else {

        for {
          tgt <- baseCell.target
          sp <- tgt.spine
          lvs = sp.map((cell: CellType) => {
            new LayoutMarker(
              new EdgeStartMarker(cell),
              new EdgeStartMarker(cell),
              true
            )
          })
          baseLayout <- baseCell.layout(lvs)
        } yield {

          for { l <- lvs } yield {
            l.rootEdge.rootY = baseCell.y - (fromInt(2) * externalPadding)
          }

          // Set the position of the outgoing edge
          baseLayout.rootEdge.endMarker.rootY =
            baseCell.rootY + (fromInt(2) * externalPadding)

          bounds

        }

      }

    }

  }

}
