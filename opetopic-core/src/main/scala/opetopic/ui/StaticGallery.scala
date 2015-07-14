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

trait StaticGalleryFramework[U] { 
  frmwk : RenderingFramework[U] with PanelFramework[U] with GalleryFramework[U] with StaticPanelFramework[U] =>

  import isNumeric._

  class StaticGallery[A[_ <: Nat], E <: ElementType](cfg: GalleryConfig)(val complex : FiniteComplex[A])(implicit r: RenderableFamily[A, E]) 
      extends Gallery[A, E](cfg) {

    type PanelType[N <: Nat] = StaticPanel[A[N], E, N]

    def createObjectPanel(nst: Nesting[A[_0], _0]) : PanelType[_0] = 
      new StaticObjectPanel[A[_0], E](cfg.panelConfig)(nst)(r(Z))

    def createNestingPanel[P <: Nat](p: P)(bn: Nesting[A[S[P]], S[P]], en: Nesting[A[P], P]) : PanelType[S[P]] = 
      new StaticNestingPanel[A[S[P]], A[P], E, P](cfg.panelConfig)(bn, Some(en))(r(S(p)))

    val panels : Suite[PanelType, S[complex.N]] = 
      createPanels(complex.n)(complex.value)

    val (galleryElement, galleryBounds) : (ElementType, BBox) = {

      val panelData : List[(ElementType, BBox)] = 
        extractPanelData(panels).reverse

      val maxHeight : U = (panelData map (_._2.height)).max

      var xPos : U = cfg.spacing

      // Now you should translate them into place
      val locatedElements : List[ElementType] = 
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
        new BBox {
          val x: U = zero
          val y: U = -maxHeight
          val width: U = xPos
          val height: U = maxHeight
        }

      (group(locatedElements : _*), bbox)

    }

    def element : ElementType = galleryElement
    def bounds: BBox = galleryBounds

  }

  //============================================================================================
  // CONSTRUCTOR
  //

  object StaticGallery {

    def apply[A[_ <: Nat], E <: ElementType](cmplx: FiniteComplex[A])(implicit r: RenderableFamily[A, E]) : StaticGallery[A, E] = 
      new StaticGallery(defaultGalleryConfig)(cmplx)

  }

}

