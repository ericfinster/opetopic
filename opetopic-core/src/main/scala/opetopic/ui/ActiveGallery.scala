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

  trait ActiveGallery[A[_ <: Nat], E <: Element] 
      extends ComplexGallery[A, E] 
      with SelectableGallery[A, E] {

    type PanelType[N <: Nat] <: ActiveGalleryPanel[N]

    val galleryGroup = group 
    def element: Element = galleryGroup
    def bounds: Bounds = elementsAndBounds._2

    trait ActiveGalleryPanel[N <: Nat] 
        extends ActivePanel[A[N], E, N] with ComplexPanel[N] {

      type BoxType <: ActiveGalleryCellBox

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

      trait ActiveGalleryCellBox extends ActiveCellBox {

        var faceComplex : Option[Complex[PanelBoxType, N]] = None

        boxRect.onMouseOver = { (e: UIEventType) => { hoverFaces } }
        boxRect.onMouseOut = { (e: UIEventType) => { unhoverFaces } }

        def hover : Unit = boxRect.fill = "red"
        def unhover : Unit = boxRect.fill = "white"

        def hoverFaces : Unit = 
          foreachFace(new IndexedOp[PanelBoxType] {
            def apply[N <: Nat](n: N)(pb: PanelBoxType[N]) = pb.hover
          })

        def unhoverFaces : Unit = 
          foreachFace(new IndexedOp[PanelBoxType] {
            def apply[N <: Nat](n: N)(pb: PanelBoxType[N]) = pb.unhover
          })

        def foreachFace(op: IndexedOp[PanelBoxType]) : Unit = 
          for {
            fc <- faceComplex
          } {
            fc.foreach(op)
          }

      }

    }

    def refreshFaceComplexes : Unit = {

      val bxcmplx = boxComplex

      bxcmplx.foreach(
        new IndexedOp[PanelBoxType] {
          def apply[N <: Nat](n: N)(pb: PanelBoxType[N]) = {
            for {
              diff <- fromOpt(diffOpt(n, bxcmplx.n))
              res <- bxcmplx.sourceAt(pb.address)(diff)
            } { pb.faceComplex = Some(res) }
          }
        }
      )
    }

  }

  class SimpleActiveGallery[A[_ <: Nat], E <: Element](val config: GalleryConfig, val complex: FiniteComplex[A])(
    implicit r: AffixableFamily[A, E]
  ) extends ActiveGallery[A, E] { thisGallery =>

    type PanelType[N <: Nat] = SimpleActiveGalleryPanel[N]

    var selection : Option[Selection] = None

    val panels : NonemptySuite[PanelType] =
      createPanels(complex.n)(complex.value)

    //Have to put the panel elements in the gallery group
    galleryGroup.children = elementsAndBounds._1
    refreshFaceComplexes

    def createObjectPanel(nst: Nesting[A[_0], _0]) : PanelType[_0] = 
      new SimpleActiveGalleryObjectPanel(nst)

    def createNestingPanel[P <: Nat](p: P)(bn: Nesting[A[S[P]], S[P]], en: Nesting[A[P], P]) : PanelType[S[P]] = 
      new SimpleActiveGalleryNestingPanel(p)(bn, Some(en))

    abstract class SimpleActiveGalleryPanel[N <: Nat] 
        extends ActiveGalleryPanel[N] {

      type BoxType = SimpleActiveGalleryCellBox
      type EdgeType = SimpleActiveGalleryCellEdge

      def cellBox(lbl: A[N], addr: Address[S[N]], isExt: Boolean) : BoxType =
        new SimpleActiveGalleryCellBox(lbl, addr, isExt)

      def cellEdge : EdgeType =
        new SimpleActiveGalleryCellEdge

      class SimpleActiveGalleryCellBox(val label: A[N], val address: Address[S[N]], val isExternal: Boolean) 
          extends ActiveGalleryCellBox {

        val decoration = affixable.decoration(label)
        makeMouseInvisible(labelElement)

      }

      class SimpleActiveGalleryCellEdge extends ActiveCellEdge

    }

    class SimpleActiveGalleryObjectPanel(val nesting: Nesting[A[_0], _0])
        extends SimpleActiveGalleryPanel[_0] with ActiveObjectPanel[A[_0], E] { 

      def panelDim = Z
      val affixable = r(Z)
      val config = thisGallery.config.panelConfig
      val boxNesting = generateBoxes(nesting.dim)(nesting)

      refresh

    }

    class SimpleActiveGalleryNestingPanel[P <: Nat](p: P)(
      val nesting: Nesting[A[S[P]], S[P]], edgeOpt: Option[Nesting[A[P], P]]
    ) extends SimpleActiveGalleryPanel[S[P]] with ActiveNestingPanel[A[S[P]], E, P] {

      def panelDim = S(p)
      val affixable = r(S(p))
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

    def apply[A[_ <: Nat], E <: Element](cmplx: FiniteComplex[A])(implicit cfg: GalleryConfig, r: AffixableFamily[A, E]) : ActiveGallery[A, E] = 
      new SimpleActiveGallery(cfg, cmplx)

  }


}
