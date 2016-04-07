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
    val panelConfig: PanelConfig = PanelConfig(),
    val width: Size = fromInt(900),
    val height: Size = fromInt(300),
    val spacing: Size = fromInt(2000),
    val minViewX: Option[Size] = None,
    val minViewY: Option[Size] = None,
    val spacerBounds: Bounds = Bounds(zero, zero, fromInt(600), fromInt(600)),
    val manageViewport: Boolean = false
  )
  
  trait Gallery[A[_ <: Nat]] {

    def config: GalleryConfig

    type GalleryPanelType[N <: Nat] <: GalleryPanel[N]
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

    def panels: NonemptySuite[GalleryPanelType]

    def extractPanelData[N <: Nat](panels: Suite[GalleryPanelType, N]) : List[(Element, Bounds)] = {
      panels.fold(
        new IndexedFold[GalleryPanelType, List[(Element, Bounds)]] {

          def caseZero : List[(Element, Bounds)] = Nil

          def caseSucc[P <: Nat](p: P)(panel: GalleryPanelType[P], lst: List[(Element, Bounds)]) : List[(Element, Bounds)] =
            (panel.element, panel.bounds) :: lst

        }
      )
    }

    def elementsAndBounds : (List[Element], Bounds) = {

      val panelData : List[(Element, Bounds)] = 
        extractPanelData(panels.value).reverse

      val maxHeight : Size = (panelData map (_._2.height)).max

      var xPos : Size = config.spacing

      // Now you should translate them into place
      val locatedElements : List[Element] = 
        for {
          (el, bnds) <- panelData
        } yield {

          val xTrans = (-bnds.x) + xPos
          val yTrans = (-bnds.y) - bnds.height - half(maxHeight - bnds.height)

          val locatedElement = 
            translate(el, xTrans, yTrans)

          xPos = xPos + bnds.width + config.spacing

          locatedElement

        }

      val (viewY, viewHeight) = 
        config.minViewY match {
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
        config.minViewX match {
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
