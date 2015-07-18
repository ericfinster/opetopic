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
    val spacing : Size
  )

  trait Gallery[A[_ <: Nat], E <: Element] {

    val config: GalleryConfig
    import config._

    def complex: FiniteComplex[A]
    def panels: NonemptySuite[PanelType]

    type AComplex[N <: Nat] = Complex[A, N]
    type PanelType[N <: Nat] <: Panel[A[N], E, N]
    type PanelBoxType[N <: Nat] = PanelType[N]#BoxType

    def createObjectPanel(nst: Nesting[A[_0], _0]) : PanelType[_0]
    def createNestingPanel[P <: Nat](p: P)(bn: Nesting[A[S[P]], S[P]], en: Nesting[A[P], P]) : PanelType[S[P]]

    @natElim
    def createPanels[N <: Nat](n: N)(cmplx: Complex[A, N]) : Suite[PanelType, S[N]] = {
      case (Z, Complex(_, objNst)) => SNil[PanelType] >> createObjectPanel(objNst)
      case (S(p), Complex(tl, hd)) => createPanels(p)(tl) >> createNestingPanel(p)(hd, tl.head)
    }

    def extractPanelData[N <: Nat](panels: Suite[PanelType, N]) : List[(Element, Bounds)] = panels.fold(
      new IndexedFold[PanelType, List[(Element, Bounds)]] {

        def caseZero : List[(Element, Bounds)] = Nil

        def caseSucc[P <: Nat](p: P)(panel: PanelType[P], lst: List[(Element, Bounds)]) : List[(Element, Bounds)] = 
          (panel.element, panel.bounds) :: lst

      }
    )

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
