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

trait HasStaticGalleries extends HasStaticPanels with HasComplexGalleries { self: UIFramework with HasGalleries =>

  import isNumeric._

  // class StaticGallery[A[_ <: Nat], E <: Element](val config: GalleryConfig, val complex : FiniteComplex[A])(implicit r: AffixableFamily[A, E])
  //     extends ComplexGallery[A, E] {

  //   type PanelType[N <: Nat] = StaticPanel[A[N], E, N]

  //   def createObjectPanel(nst: Nesting[A[_0], _0]) : PanelType[_0] =
  //     new StaticObjectPanel[A[_0], E](config.panelConfig, nst)(r(Z))

  //   def createNestingPanel[P <: Nat](p: P)(bn: Nesting[A[S[P]], S[P]], en: Nesting[A[P], P]) : PanelType[S[P]] =
  //     new StaticNestingPanel[A[S[P]], A[P], E, P](p)(config.panelConfig, bn, Some(en))(r(S(p)))



  //   val panels : NonemptySuite[PanelType] =
  //     createPanels(complex.n)(complex.value)

  //   val (galleryElement, galleryBounds) : (Element, Bounds) = {
  //     val (panelEls, bnds) = elementsAndBounds
  //     (group(panelEls: _*), bnds)
  //   }

  //   def element : Element = galleryElement
  //   def bounds: Bounds = galleryBounds

  // }

  // //============================================================================================
  // // CONSTRUCTOR
  // //

  // object StaticGallery {

  //   def apply[A[_ <: Nat], E <: Element](cmplx: FiniteComplex[A])(implicit cfg: GalleryConfig, r: AffixableFamily[A, E]) : StaticGallery[A, E] =
  //     new StaticGallery(cfg, cmplx)

  // }

}
