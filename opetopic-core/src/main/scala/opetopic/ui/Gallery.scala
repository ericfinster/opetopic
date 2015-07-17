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

trait GalleryFramework[U] { frmwk: RenderingFramework[U] with PanelFramework[U] =>

  import isNumeric._

  case class GalleryConfig(
    val panelConfig: PanelConfig,
    val spacing : U
  )

  def defaultGalleryConfig: GalleryConfig

  abstract class Gallery[A[_ <: Nat], E <: Element](cfg: GalleryConfig)(implicit r: AffixableFamily[A, E]) 
      extends BoundedElement[Element] { thisGallery =>

    def complex: FiniteComplex[A]

    type AComplex[N <: Nat] = Complex[A, N]
    type PanelType[N <: Nat] <: Panel[A[N], E, N]

    def createObjectPanel(nst: Nesting[A[_0], _0]) : PanelType[_0]
    def createNestingPanel[P <: Nat](p: P)(bn: Nesting[A[S[P]], S[P]], en: Nesting[A[P], P]) : PanelType[S[P]]

    @natElim
    def createPanels[N <: Nat](n: N)(cmplx: Complex[A, N]) : Suite[PanelType, S[N]] = {
      case (Z, Complex(_, objNst)) => SNil[PanelType] >> createObjectPanel(objNst)
      case (S(p), Complex(tl, hd)) => createPanels(p)(tl) >> createNestingPanel(p)(hd, tl.head)
    }

    def extractPanelData[N <: Nat](panels: Suite[PanelType, N]) : List[(Element, BBox)] = panels.fold(
      new IndexedFold[PanelType, List[(Element, BBox)]] {

        def caseZero : List[(Element, BBox)] = Nil

        def caseSucc[P <: Nat](p: P)(panel: PanelType[P], lst: List[(Element, BBox)]) : List[(Element, BBox)] = 
          (panel.element, panel.bounds) :: lst

      }
    )

  }

}
