/**
  * Gallery.scala - Opetopic Galleries
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import syntax.suite._
import syntax.complex._

trait HasGalleries extends HasPanels { self: UIFramework => 

  import isNumeric._

  case class GalleryConfig(
    val panelConfig: PanelConfig,
    val width: Size,
    val height: Size,
    val spacing: Size,
    val minViewX: Option[Size],
    val minViewY: Option[Size],
    val spacerBounds: Bounds
  )

  trait Gallery[A[_ <: Nat]] {

    val config: GalleryConfig
    import config._

    type PanelType[N <: Nat] <: GalleryPanel[N]
    type GalleryBoxType[N <: Nat] <: CellBox[A[N], N] { type BoxAddressType = GalleryAddressType[N] }
    type GalleryEdgeType[N <: Nat] <: CellEdge[A[N], N]
    type GalleryAddressType[N <: Nat]
    type GalleryLabelType <: Element

    trait GalleryPanel[N <: Nat] extends Panel[A[N], N] { 

      type PanelAddressType = GalleryAddressType[N]
      type LabelElementType = GalleryLabelType

      type BoxType <: GalleryBoxType[N] 
      type EdgeType <: GalleryEdgeType[N]

    }

    def panels: NonemptySuite[PanelType]

    def extractPanelData[N <: Nat](panels: Suite[PanelType, N]) : List[(Element, Bounds)] = {
      panels.fold(
        new IndexedFold[PanelType, List[(Element, Bounds)]] {

          def caseZero : List[(Element, Bounds)] = Nil

          def caseSucc[P <: Nat](p: P)(panel: PanelType[P], lst: List[(Element, Bounds)]) : List[(Element, Bounds)] =
            (panel.element, panel.bounds) :: lst

        }
      )
    }

    def elementsAndBounds : (List[Element], Bounds) = {

      val panelData : List[(Element, Bounds)] = 
        extractPanelData(panels.value).reverse

      val maxHeight : Size = (panelData map (_._2.height)).max

      var xPos : Size = spacing

      // Now you should translate them into place
      val locatedElements : List[Element] = 
        for {
          (el, bnds) <- panelData
        } yield {

          val xTrans = (-bnds.x) + xPos
          val yTrans = (-bnds.y) - bnds.height - half(maxHeight - bnds.height)

          val locatedElement = 
            translate(el, xTrans, yTrans)

          xPos = xPos + bnds.width + spacing

          locatedElement

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

      val bbox : Bounds = 
        Bounds(zero, viewY, xPos, viewHeight)

      (locatedElements, bbox)

    }

  }

}
