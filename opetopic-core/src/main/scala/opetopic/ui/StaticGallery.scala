/**
  * StaticGallery.scala - A Static Gallery
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import syntax.complex._
import syntax.nesting._
import syntax.suite._
import TypeLemmas._

trait HasStaticGalleries extends HasStaticPanels with HasComplexGalleries { 
  self: UIFramework with HasGalleries =>

  import isNumeric._

  trait StaticGallery[A[_ <: Nat]] extends ComplexGallery[A] {

    type GalleryPanelType[N <: Nat] <: StaticGalleryPanel[N]
    type GalleryBoxType[N <: Nat] <: StaticGalleryCellBox[A[N], N] 
    type GalleryEdgeType[N <: Nat] <: StaticCellEdge[A[N], N]

    trait StaticGalleryPanel[N <: Nat] extends StaticPanel[A[N], N] with ComplexPanel[N] 
    trait StaticGalleryCellBox[A, N <: Nat] extends StaticCellBox[A, N] {

      type BoxAddressType = Address[S[N]]

      def nestingAddress = address

    }

  }

  class SimpleStaticGallery[A[_ <: Nat]](val config: GalleryConfig, val complex: FiniteComplex[A])(
    implicit v: VisualizableFamily[A]
  ) extends StaticGallery[A] { thisGallery => 

    type GalleryPanelType[N <: Nat] = SimpleStaticGalleryPanel[N]
    type GalleryBoxType[N <: Nat] = SimpleStaticGalleryCellBox[N] 
    type GalleryEdgeType[N <: Nat] = SimpleStaticGalleryCellEdge[N]

    def element : Element = galleryElement
    def bounds: Bounds = galleryBounds

    val panels : NonemptySuite[SimpleStaticGalleryPanel] =
      createPanels(complex.n)(complex.value)

    val (galleryElement, galleryBounds) : (Element, Bounds) = {
      val (panelEls, bnds) = elementsAndBounds
      (viewport(config.width, config.height, bnds, panelEls: _*), bnds)
    }

    def createObjectPanel(nst: Nesting[A[_0], _0]) : GalleryPanelType[_0] =
      new SimpleStaticGalleryObjectPanel(nst)

    def createNestingPanel[P <: Nat](p: P)(bn: Nesting[A[S[P]], S[P]], en: Nesting[A[P], P]) : GalleryPanelType[S[P]] =
      new SimpleStaticGalleryNestingPanel(p)(bn, Some(en))

    abstract class SimpleStaticGalleryPanel[N <: Nat]
        extends StaticGalleryPanel[N] {

      type BoxType = SimpleStaticGalleryCellBox[N]
      type EdgeType = SimpleStaticGalleryCellEdge[N]

      def cellBox(lbl: A[N], addr: Address[S[N]], isExt: Boolean) : BoxType =
        new SimpleStaticGalleryCellBox(this, lbl, addr, isExt)

      def cellEdge : EdgeType =
        new SimpleStaticGalleryCellEdge(this)

      def visualize(a: A[N]) : Visualization[N] = 
        v.visualize(panelDim)(a)

    }

    class SimpleStaticGalleryCellBox[N <: Nat](
      val panel: SimpleStaticGalleryPanel[N],
      val label: A[N],
      val address: Address[S[N]],
      val isExternal: Boolean
    ) extends StaticGalleryCellBox[A[N], N] {

      type PanelType = SimpleStaticGalleryPanel[N]

      val visualization = panel.visualize(label)
      makeMouseInvisible(labelElement)

    }

    class SimpleStaticGalleryCellEdge[N <: Nat](
      val panel: SimpleStaticGalleryPanel[N]
    ) extends StaticCellEdge[A[N], N]


    class SimpleStaticGalleryObjectPanel(val nesting: Nesting[A[_0], _0])
        extends SimpleStaticGalleryPanel[_0] with StaticObjectPanel[A[_0]] {

      def panelDim = Z
      val config = thisGallery.config.panelConfig
      val boxNesting = generateBoxes(nesting.dim)(nesting)

      layoutObjects(boxNesting)

    }

    class SimpleStaticGalleryNestingPanel[P <: Nat](p: P)(
      val nesting: Nesting[A[S[P]], S[P]], edgeOpt: Option[Nesting[A[P], P]]
    ) extends SimpleStaticGalleryPanel[S[P]] with StaticNestingPanel[A[S[P]], P] {

      def panelDim = S(p)
      val config = thisGallery.config.panelConfig
      val boxNesting = generateBoxes(nesting.dim)(nesting)

      val edgeNesting : Nesting[EdgeType, P] =
        edgeOpt match {
          case None => reconstructEdges(boxNesting.dim.pred)(boxNesting)
          case Some(et) => connectEdges(et map (_ => cellEdge), boxNesting)
        }

      layout(boxNesting, edgeNesting)

    }

  }

  //============================================================================================
  // CONSTRUCTOR
  //

  object SimpleStaticGallery {

    def apply[A[_ <: Nat]](cmplx: FiniteComplex[A])(
      implicit cfg: GalleryConfig, v: VisualizableFamily[A]
    ) : SimpleStaticGallery[A] =
      new SimpleStaticGallery(cfg, cmplx)

  }

}
