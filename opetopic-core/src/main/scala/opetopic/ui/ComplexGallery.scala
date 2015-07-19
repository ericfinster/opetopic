/**
  * ComplexGallery.scala - A Gallery for Displaying a complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import syntax.suite._
import syntax.complex._
import syntax.nesting._

trait HasComplexGalleries { self: UIFramework with HasPanels with HasGalleries => 

  import isNumeric._

  trait ComplexGallery[A[_ <: Nat], E <: Element] extends Gallery[A, E] {

    def complex: FiniteComplex[A]

    type AComplex[N <: Nat] = Complex[A, N]

    type PanelType[N <: Nat] <: Panel[A[N], E, N] with ComplexPanel[N]
    type PanelAddressType[N <: Nat] = Address[S[N]]

    def createObjectPanel(nst: Nesting[A[_0], _0]) : PanelType[_0]
    def createNestingPanel[P <: Nat](p: P)(bn: Nesting[A[S[P]], S[P]], en: Nesting[A[P], P]) : PanelType[S[P]]

    @natElim
    def createPanels[N <: Nat](n: N)(cmplx: Complex[A, N]) : Suite[PanelType, S[N]] = {
      case (Z, Complex(_, objNst)) => SNil[PanelType] >> createObjectPanel(objNst)
      case (S(p), Complex(tl, hd)) => createPanels(p)(tl) >> createNestingPanel(p)(hd, tl.head)
    }

    trait ComplexPanel[N <: Nat] extends GalleryPanel[N] { self: Panel[A[N], E, N] =>

      def seekToAddress(addr: Address[S[N]]) : ShapeM[NestingZipper[BoxType, N]] = 
        boxNesting.seekTo(addr)

    }

    def boxComplex : FiniteComplex[PanelBoxType] = {

      type NestingType[N <: Nat] = Nesting[PanelBoxType[N], N]

      val ps = panels

      val res : Suite[NestingType, S[ps.P]] = ps.map(
        new IndexedMap[PanelType, NestingType] {
          def apply[N <: Nat](n: N)(p: PanelType[N]) : NestingType[N] = 
            p.boxNesting
        }
      )

      complexToFiniteComplex[PanelBoxType, ps.P](res)

    }

  }

}
