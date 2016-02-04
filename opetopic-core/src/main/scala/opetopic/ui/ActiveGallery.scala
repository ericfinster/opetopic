/**
  * ActiveGallery.scala - Active Opetopic Galleries
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import syntax.suite._
import syntax.nesting._
import syntax.complex._
import TypeLemmas._

trait HasActiveGalleries extends HasActivePanels with HasComplexGalleries { 
  self: ActiveFramework with HasSelectableGalleries =>

  import isNumeric._

  trait ActiveGallery[A[_ <: Nat]] extends ComplexGallery[A] with SelectableGallery[A] {

    type GalleryPanelType[N <: Nat] <: ActiveGalleryPanel[N]
    type GalleryBoxType[N <: Nat] <: ActiveGalleryCellBox[A[N], N]
    type GalleryEdgeType[N <: Nat] <: ActiveCellEdge[A[N], N]

    val galleryViewport = viewport
    def element: Element = galleryViewport
    var bounds: Bounds = Bounds(zero,zero,zero,zero)

    def refreshGallery : Unit = {
      val (panelEls, bnds) = elementsAndBounds
      galleryViewport.width = config.width
      galleryViewport.height = config.height
      galleryViewport.setBounds(bnds)
      galleryViewport.children = panelEls
      bounds = bnds
    }

    trait ActiveGalleryPanel[N <: Nat] extends ActivePanel[A[N], N] with ComplexPanel[N] {

      def cellBox(lbl: A[N], addr: Address[S[N]], isExt: Boolean) : BoxType

      //============================================================================================
      // INITIALIZATION
      //

      def generateBoxes(n: N)(nst: Nesting[A[N], N]) : Nesting[BoxType, N] =
        Nesting.elimWithAddress[A[N], Nesting[BoxType, N], N](n)(nst)({
          case (a, addr) => Nesting.external(n)(cellBox(a, addr, true))
        })({
          case (a, addr, cn) => Box(cellBox(a, addr, false), cn)
        })

    }

    trait ActiveGalleryCellBox[A, N <: Nat] extends ActiveCellBox[A, N] {

      type BoxAddressType = Address[S[N]]

      var faceComplex : Option[Complex[GalleryBoxType, N]] = None

      boxRect.onMouseOver = { (e: UIMouseEvent) => { hoverFaces } }
      boxRect.onMouseOut = { (e: UIMouseEvent) => { unhoverFaces } }

      def nestingAddress = address

      def hoverFaces : Unit =
        foreachFace(new IndexedOp[GalleryBoxType] {
          def apply[N <: Nat](n: N)(pb: GalleryBoxType[N]) = pb.setHoveredStyle
        })

      def unhoverFaces : Unit =
        foreachFace(new IndexedOp[GalleryBoxType] {
          def apply[N <: Nat](n: N)(pb: GalleryBoxType[N]) = pb.setUnhoveredStyle
        })

      def foreachFace(op: IndexedOp[GalleryBoxType]) : Unit =
        for {
          fc <- faceComplex
        } {
          fc.foreach(op)
        }

    }

    def refreshFaceComplexes : Unit = {

      val bxcmplx = boxComplex

      bxcmplx.foreach(
        new IndexedOp[GalleryBoxType] {
          def apply[N <: Nat](n: N)(pb: GalleryBoxType[N]) = {
            for {
              diff <- fromOpt(diffOpt(n, bxcmplx.n))
              res <- bxcmplx.sourceAt(pb.address)(diff)
            } { pb.faceComplex = Some(res) }
          }
        }
      )
    }

  }

  class SimpleActiveGallery[A[_ <: Nat]](val config: GalleryConfig, val complex: FiniteComplex[A])(
    implicit v: VisualizableFamily[A]
  ) extends ActiveGallery[A] { thisGallery =>

    type GalleryPanelType[N <: Nat] = SimpleActiveGalleryPanel[N]
    type GalleryBoxType[N <: Nat] = SimpleActiveGalleryCellBox[N]
    type GalleryEdgeType[N <: Nat] = SimpleActiveGalleryCellEdge[N]

    var selection : Option[Selection] = None

    val panels : NonemptySuite[GalleryPanelType] =
      createPanels(complex.n)(complex.value)

    refreshGallery

    def createObjectPanel(nst: Nesting[A[_0], _0]) : GalleryPanelType[_0] =
      new SimpleActiveGalleryObjectPanel(nst)

    def createNestingPanel[P <: Nat](p: P)(bn: Nesting[A[S[P]], S[P]], en: Nesting[A[P], P]) : GalleryPanelType[S[P]] =
      new SimpleActiveGalleryNestingPanel(p)(bn, Some(en))

    abstract class SimpleActiveGalleryPanel[N <: Nat]
        extends ActiveGalleryPanel[N] {

      type BoxType = SimpleActiveGalleryCellBox[N]
      type EdgeType = SimpleActiveGalleryCellEdge[N]

      def cellBox(lbl: A[N], addr: Address[S[N]], isExt: Boolean) : BoxType =
        new SimpleActiveGalleryCellBox(this, lbl, addr, isExt)

      def cellEdge : EdgeType =
        new SimpleActiveGalleryCellEdge(this)

      def visualize(a: A[N]) : Visualization[N] = 
        v.visualize(panelDim)(a)

    }

    class SimpleActiveGalleryCellBox[N <: Nat](
      val panel: SimpleActiveGalleryPanel[N],
      var label: A[N],
      val address: Address[S[N]],
      val isExternal: Boolean
    ) extends ActiveGalleryCellBox[A[N], N] {

      type PanelType = SimpleActiveGalleryPanel[N]

      val visualization = panel.visualize(label)
      makeMouseInvisible(labelElement)

    }

    class SimpleActiveGalleryCellEdge[N <: Nat](
      val panel: SimpleActiveGalleryPanel[N]
    ) extends ActiveCellEdge[A[N], N]

    class SimpleActiveGalleryObjectPanel(val nesting: Nesting[A[_0], _0])
        extends SimpleActiveGalleryPanel[_0] with ActiveObjectPanel[A[_0]] {

      def panelDim = Z
      val config = thisGallery.config.panelConfig
      val boxNesting = generateBoxes(nesting.dim)(nesting)

      refresh

    }

    class SimpleActiveGalleryNestingPanel[P <: Nat](p: P)(
      val nesting: Nesting[A[S[P]], S[P]], edgeOpt: Option[Nesting[A[P], P]]
    ) extends SimpleActiveGalleryPanel[S[P]] with ActiveNestingPanel[A[S[P]], P] {

      def panelDim = S(p)
      val config = thisGallery.config.panelConfig
      val boxNesting = generateBoxes(nesting.dim)(nesting)

      val edgeNesting : Nesting[EdgeType, P] =
        edgeOpt match {
          case None => reconstructEdges(boxNesting.dim.pred)(boxNesting)
          case Some(et) => connectEdges(et map (_ => cellEdge), boxNesting)
        }

      refresh

    }

  }

  //============================================================================================
  // CONSTRUCTOR
  //

  object ActiveGallery {

    def apply[A[_ <: Nat]](cmplx: FiniteComplex[A])(implicit cfg: GalleryConfig, v: VisualizableFamily[A]) : ActiveGallery[A] = 
      new SimpleActiveGallery(cfg, cmplx)

    def apply[A[_ <: Nat]](cfg: GalleryConfig, cmplx: FiniteComplex[A])(implicit v: VisualizableFamily[A]) : ActiveGallery[A] = 
      new SimpleActiveGallery(cfg, cmplx)

  }

}
