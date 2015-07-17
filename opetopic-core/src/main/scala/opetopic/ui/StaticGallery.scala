/**
  * StaticGallery.scala - A Static Gallery
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import syntax.complex._
import syntax.suite._
import TypeLemmas._

abstract class StaticGalleryFramework[U](implicit isNumeric: Numeric[U], isOrdered: Ordering[U])
  extends StaticPanelFramework[U] 
  with GalleryFramework[U] {

  import isNumeric._

  class StaticGallery[A[_ <: Nat], E <: Element](cfg: GalleryConfig)(val complex : FiniteComplex[A])(implicit r: AffixableFamily[A, E]) 
      extends Gallery[A, E](cfg) {

    type PanelType[N <: Nat] = StaticPanel[A[N], E, N]

    def createObjectPanel(nst: Nesting[A[_0], _0]) : PanelType[_0] = 
      new StaticObjectPanel[A[_0], E](cfg.panelConfig)(nst)(r(Z))

    def createNestingPanel[P <: Nat](p: P)(bn: Nesting[A[S[P]], S[P]], en: Nesting[A[P], P]) : PanelType[S[P]] = 
      new StaticNestingPanel[A[S[P]], A[P], E, P](cfg.panelConfig)(bn, Some(en))(r(S(p)))

    val panels : Suite[PanelType, S[complex.N]] = 
      createPanels(complex.n)(complex.value)

    val (galleryElement, galleryBounds) : (Element, BBox) = {

      val panelData : List[(Element, BBox)] = 
        extractPanelData(panels).reverse

      val maxHeight : U = (panelData map (_._2.height)).max

      var xPos : U = cfg.spacing

      // Now you should translate them into place
      val locatedElements : List[Element] = 
        for {
          (el, bnds) <- panelData
        } yield {

          val xTrans = (-bnds.x) + xPos
          val yTrans = (-bnds.y) - bnds.height - half(maxHeight - bnds.height)

          val locatedElement = 
            translate(el, xTrans, yTrans)

          xPos = xPos + bnds.width + cfg.spacing

          locatedElement

        }

      val bbox : BBox = 
        BBox(zero, -maxHeight, xPos, maxHeight)

      (group(locatedElements : _*), bbox)

    }

    def element : Element = galleryElement
    def bounds: BBox = galleryBounds

  }

  //============================================================================================
  // CONSTRUCTOR
  //

  object StaticGallery {

    def apply[A[_ <: Nat], E <: Element](cmplx: FiniteComplex[A])(implicit r: AffixableFamily[A, E]) : StaticGallery[A, E] = 
      new StaticGallery(defaultGalleryConfig)(cmplx)

  }

}

