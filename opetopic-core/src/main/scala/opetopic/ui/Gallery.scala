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
    val spacing : Size
  )

  trait Gallery[A[_ <: Nat], E <: Element] {

    val config: GalleryConfig
    import config._

    type PanelType[N <: Nat] <: Panel[A[N], E, N] with GalleryPanel[N]
    type PanelBoxType[N <: Nat] = PanelType[N]#BoxType
    type PanelAddressType[N <: Nat]

    trait GalleryPanel[N <: Nat] { thisPanel: Panel[A[N], E, N] =>

      type AddressType = PanelAddressType[N]

    }

    def panels: NonemptySuite[PanelType]

    def extractPanelData[N <: Nat](panels: Suite[PanelType, N]) : List[(Element, Bounds)] = panels.fold(
      new IndexedFold[PanelType, List[(Element, Bounds)]] {

        def caseZero : List[(Element, Bounds)] = Nil

        def caseSucc[P <: Nat](p: P)(panel: PanelType[P], lst: List[(Element, Bounds)]) : List[(Element, Bounds)] = 
          (panel.element, panel.bounds) :: lst

      }
    )

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

      val bbox : Bounds = 
        Bounds(zero, -maxHeight, xPos, maxHeight)

      (locatedElements, bbox)

    }

  }

}
