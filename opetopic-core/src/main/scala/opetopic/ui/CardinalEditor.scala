/**
  * CardinalEditor.scala - Cardinal Editor Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scala.language.existentials

import opetopic._
import syntax.suite._
import syntax.nesting._
import syntax.complex._
import syntax.cardinal._
import TypeLemmas._

trait HasEditor extends { self: ActiveFramework with HasActivePanels with HasSelectableGalleries => 

  import isNumeric._

  class CardinalEditor[A[_ <: Nat]](c: FiniteCardinal[Lambda[`N <: Nat` => Option[A[N]]]])(
    implicit val v: VisualizableFamily[A]
  ) extends SelectableGallery[Lambda[`N <: Nat` => Polarity[Option[A[N]]]]] { thisEditor =>

    type OptA[N <: Nat] = Option[A[N]]
    type PolOptA[N <: Nat] = Polarity[Option[A[N]]]

    type GalleryPanelType[N <: Nat] = CardinalPanel[N]
    type PanelSuite[N <: Nat] = Suite[GalleryPanelType, S[N]]
    type GalleryBoxType[N <: Nat] = CardinalCellBox[N]
    type GalleryEdgeType[N <: Nat] = CardinalCellEdge[N]
    type GalleryAddressType[N <: Nat] = CardinalAddress[S[N]]
    type GalleryLabelType = Element

    var config = DefaultGalleryConfig

    var panels: NonemptySuite[CardinalPanel] = 
      createPanels(c.n)(c.value)

    var selection: Option[Selection] = None

    val galleryViewport = viewport
    def element: Element = galleryViewport
    var bounds = Bounds(zero, zero, zero, zero)

    def refreshGallery : Unit = {
      val (panelEls, bnds) = elementsAndBounds
      galleryViewport.width = config.width
      galleryViewport.height = config.height
      galleryViewport.setBounds(bnds)
      galleryViewport.children = panelEls
      bounds = bnds
    }

    refreshGallery

    @natElim
    def createPanels[N <: Nat](n: N)(cardinal: Cardinal[OptA, N]) : Suite[CardinalPanel, S[N]] = {
      case (Z, Cardinal(_, objNst)) => SNil[CardinalPanel] >> new CardinalObjectPanel(objNst)
      case (S(p: P), Cardinal(tl, hd)) => {
        val newTail = createPanels(p)(tl) 
        val newHead = new CardinalNestingPanel(p)(hd, newTail.head)
        newTail >> newHead
      }
    }

    def boxCardinal[N <: Nat](ps: PanelSuite[N]) : Cardinal[NeutralCellBox, N] = {

      type NestingType[K <: Nat] = CardinalNesting[NeutralCellBox[K], K]

      ps.map(
        new IndexedMap[GalleryPanelType, NestingType] {
          def apply[K <: Nat](k: K)(p: GalleryPanelType[K]) : NestingType[K] = 
            p.cardinalBoxNesting
        }
      )

    }

    def boxComplex[N <: Nat](ps: PanelSuite[N]) : Complex[CardinalCellBox, N] = {

      type NestingType[K <: Nat] = Nesting[CardinalCellBox[K], K]

      ps.map(
        new IndexedMap[GalleryPanelType, NestingType] {
          def apply[K <: Nat](k: K)(p: GalleryPanelType[K]) : NestingType[K] = 
            p.boxNesting
        }
      )

    }

    //============================================================================================
    // MUTABILITY ROUTINES
    //

    def extendPanels(ps: NonemptySuite[CardinalPanel]) : NonemptySuite[CardinalPanel] = {

      implicit val p = ps.p

      val extendedNesting = ps.head.cardinalNesting map {
        case nst => Nesting.extendNesting(nst)(_ => None)
      }

      val extendedPanel = 
        new CardinalNestingPanel(p)(extendedNesting, ps.head)

      ps.value >> extendedPanel

    }

    def extrudeSelection : Unit = {

      selection match {
        case None => ()
        case Some(sel) =>
          if (! sel.root.isExtrudable) toast("Selection not extrudable") else {

            val selectionDim : Int = natToInt(sel.dim)
            val currentDim : Int = natToInt(panels.p)

            val extrusionSuite = 
              if (selectionDim == currentDim) {
                extendPanels(panels)
              } else panels

            val extrusionCardinal : Cardinal[NeutralCellBox, extrusionSuite.P] = 
              boxCardinal(extrusionSuite.value)

            val extrusionAddress : CardinalAddress[sel.Dim] = 
              Suite.tail[Address, S[sel.Dim]](sel.root.address)

            for {
              diff <- fromOpt(diffOpt(S(sel.dim), extrusionSuite.p))
              tgtPanel <- fromOpt(extrusionSuite.getOpt(sel.dim))
              fillPanel = extrusionSuite.get(S(sel.dim))(diff)
              tgtBox = tgtPanel.neutralCellBox(None, sel.root.address, false)
              fillBox = fillPanel.neutralCellBox(None, sel.root.address >> Nil, true)
              extrudedCardinal <- extrusionCardinal.extrudeSelection(sel.dim)(
                extrusionAddress, tgtBox, fillBox
              )(box => box.isSelected)(diff)
            } yield {

              type ResType[K <: Nat] = (CardinalNesting[NeutralCellBox[K], K], GalleryPanelType[K])
              type ResDim = S[extrusionSuite.P]

              deselectAll

              Suite.foreach[ResType, ResDim](extrudedCardinal.zipWith(extrusionSuite.value))(
                new IndexedOp[ResType] {
                  def apply[N <: Nat](n: N)(r: ResType[N]) = {
                    for {
                      d <- diffOpt(sel.dim, n) // Only refresh above affected dimensions
                    } {
                      val (nst, pn) = r

                      pn.cardinalBoxNesting = nst
                      pn.refreshAddresses
                      pn.refresh
                    }
                  }
                }
              )

              panels = extrusionSuite

              refreshGallery

              selectAsRoot(tgtBox)

            }
          }
      }

    }

    def extrudeDrop : Unit = {

      selection match {
        case None => ()
        case Some(sel) => 
          if (! sel.root.isExtrudable) toast("Selection not extrudable") else {

            val selectionDim : Int = natToInt(sel.dim)
            val currentDim : Int = natToInt(panels.p)

            val extrusionSuite = 
              if (selectionDim == currentDim) {
                extendPanels(extendPanels(panels))
              } else if (selectionDim == currentDim - 1) {
                extendPanels(panels)
              } else panels

            val extrusionCardinal : Cardinal[NeutralCellBox, extrusionSuite.P] = 
              boxCardinal(extrusionSuite.value)

            val extrusionAddress : CardinalAddress[sel.Dim] = 
              Suite.tail[Address, S[sel.Dim]](sel.root.address)

            for {
              diff <- fromOpt(diffOpt(S(S(sel.dim)), extrusionSuite.p))
              tgtPanel <- fromOpt(extrusionSuite.getOpt(S(sel.dim)))
              fillPanel = extrusionSuite.get(S(S(sel.dim)))(diff)
              tgtBox = tgtPanel.neutralCellBox(None, sel.root.address >> Nil, false)
              fillBox = fillPanel.neutralCellBox(None, sel.root.address >> Nil >> Nil, true)
              extrudedCardinal <- Cardinal.dropAtAddress(diff.lte)(
                extrusionCardinal, extrusionAddress, tgtBox, fillBox
              )
            } yield {

              type ResType[K <: Nat] = (CardinalNesting[NeutralCellBox[K], K], GalleryPanelType[K])
              type ResDim = S[extrusionSuite.P]

              deselectAll

              Suite.foreach[ResType, ResDim](extrudedCardinal.zipWith(extrusionSuite.value))(
                new IndexedOp[ResType] {
                  def apply[N <: Nat](n: N)(r: ResType[N]) = {
                    for {
                      d <- diffOpt(sel.dim, n) // Only refresh above affected dimensions
                    } {
                      val (nst, pn) = r

                      pn.cardinalBoxNesting = nst
                      pn.refreshAddresses
                      pn.refresh
                    }
                  }
                }
              )

              panels = extrusionSuite

              refreshGallery

              selectAsRoot(tgtBox)

            }
          }
      }

    }

    def sprout : Unit = {

      selection match {
        case None => ()
        case Some(sel) =>
          if (sel.root.isExternal) {

            val selectionDim : Int = natToInt(sel.dim)
            val currentDim : Int = natToInt(panels.p)

            val extrusionSuite = 
              if (selectionDim == currentDim) {
                extendPanels(panels)
              } else panels

            val extrusionCardinal : Cardinal[NeutralCellBox, extrusionSuite.P] =
              boxCardinal(extrusionSuite.value)

            val extrusionAddress : CardinalAddress[S[sel.Dim]] = 
              sel.root.address

            val prevAddress : CardinalAddress[sel.Dim] = 
              Suite.tail[Address, S[sel.Dim]](sel.root.address)

            for {
              diff <- fromOpt(diffOpt(S(sel.dim), extrusionSuite.p))
              tgtPanel <- fromOpt(extrusionSuite.getOpt(sel.dim))
              fillPanel = extrusionSuite.get(S(sel.dim))(diff)
              tgtBox = tgtPanel.neutralCellBox(None, extrusionAddress, true)
              fillBox = fillPanel.neutralCellBox(None, extrusionAddress >> Nil, true)
              extrudedCardinal <- Cardinal.sproutAtAddress(diff.lte)(
                extrusionCardinal, extrusionAddress, tgtBox, fillBox
              )
            } yield {

              type ResType[K <: Nat] = (CardinalNesting[NeutralCellBox[K], K], GalleryPanelType[K])
              type ResDim = S[extrusionSuite.P]

              deselectAll

              sel.root.isExternal = false

              Suite.foreach[ResType, ResDim](extrudedCardinal.zipWith(extrusionSuite.value))(
                new IndexedOp[ResType] {
                  def apply[N <: Nat](n: N)(r: ResType[N]) = {
                    for {
                      d <- diffOpt(sel.dim, n) // Only refresh above affected dimensions
                    } {
                      val (nst, pn) = r

                      pn.cardinalBoxNesting = nst
                      pn.refreshAddresses
                      pn.refresh
                    }
                  }
                }
              )

              panels = extrusionSuite

              refreshGallery

              selectAsRoot(tgtBox)

            }

          }
      }
    }

    //============================================================================================
    // CELL BOX IMPLEMENTATION
    //

    trait CardinalCellBox[N <: Nat] extends ActiveCellBox[PolOptA[N], N] {

      type Dim = N
      type PanelType = CardinalPanel[N]
      type BoxAddressType = CardinalAddress[S[N]]

      def isPolarized : Boolean
      def isExternal_=(b: Boolean): Unit

      def isExtrudable : Boolean =
        address match {
          case (_ >> Nil) => ! isPolarized
          case _ => false
        }

      var optLabel: OptA[N]

      def faceComplex : ShapeM[Complex[CardinalCellBox, N]] = {
        val ps = panels.value
        val cmplx = boxComplex(ps)
        cmplx.sourceAt(boxDim)(nestingAddress)
      }

      def labelComplex : ShapeM[Complex[OptA, N]] =
        for {
          fc <- faceComplex
        } yield fc.map(new IndexedMap[CardinalCellBox, OptA] {
          def apply[K <: Nat](k: K)(box: CardinalCellBox[K]) =
            box.optLabel
        })

    }

    class CardinalCellEdge[N <: Nat](
      val panel: CardinalPanel[N]
    ) extends ActiveCellEdge[PolOptA[N], N]


    class NeutralCellBox[N <: Nat](
      val panel: CardinalPanel[N],
      ol: OptA[N],
      addr: CardinalAddress[S[N]],
      var isExternal: Boolean
    ) extends CardinalCellBox[N] { thisBox =>

      var myOptLabel : OptA[N] = ol

      def optLabel: OptA[N] = myOptLabel
      def optLabel_=(opt: OptA[N]): Unit = {
        myOptLabel = opt
        visualization = panel.visualize(label)
        makeMouseInvisible(labelElement)
      }

      def label: PolOptA[N] = 
        Neutral(optLabel)
      def label_=(po : PolOptA[N]): Unit = ()

      val isPolarized = false
      var address: CardinalAddress[S[N]] = addr
      def nestingAddress: Address[S[N]] = 
        Cardinal.cardinalAddressComplete(S(boxDim))(address)

      var visualization = panel.visualize(label)
      makeMouseInvisible(labelElement)

      boxRect.onClick = (e: UIMouseEvent) => { thisEditor.select(thisBox) }
      boxRect.onMouseOver = { (e : UIMouseEvent) => hoverFaces }
      boxRect.onMouseOut = { (e : UIMouseEvent) => unhoverFaces }

      override def select = selectFaces
      override def deselect = deselectFaces

      def selectFaces : Unit = 
        foreachFace(new IndexedOp[GalleryBoxType] {
          def apply[N <: Nat](n: N)(pb: GalleryBoxType[N]) = {
            pb.isSelected = true
            pb.setSelectedStyle
          }
        })

      def deselectFaces : Unit = 
        foreachFace(new IndexedOp[GalleryBoxType] {
          def apply[N <: Nat](n: N)(pb: GalleryBoxType[N]) = {
            pb.isSelected = false
            pb.setDeselectedStyle
          }
        })

      def hoverFaces : Unit =
        foreachFace(new IndexedOp[GalleryBoxType] {
          def apply[N <: Nat](n: N)(pb: GalleryBoxType[N]) = 
            pb.setHoveredStyle
        })

      def unhoverFaces : Unit =
        foreachFace(new IndexedOp[GalleryBoxType] {
          def apply[N <: Nat](n: N)(pb: GalleryBoxType[N]) = 
            pb.setUnhoveredStyle
        })

      def foreachFace(op: IndexedOp[GalleryBoxType]) : Unit =
        for {
          fc <- faceComplex
        } {
          fc.foreach(op)
        }

    }

    class PositiveBox[N <: Nat](
      val panel: CardinalPanel[N]
    ) extends CardinalCellBox[N] {

      val label: PolOptA[N] = Positive()
      def label_=(po : PolOptA[N]): Unit = ()
      val address: CardinalAddress[S[N]] = null // No address for polarized cells
      val nestingAddress: Address[S[N]] = List()
      val isExternal: Boolean = false
      val isPolarized: Boolean = true
      val visualization = panel.visualize(label)
      makeMouseInvisible(labelElement)

      boxRect.onClick = { (e : UIMouseEvent) => thisEditor.deselectAll }
      boxRect.onMouseOver = { (e : UIMouseEvent) => () }
      boxRect.onMouseOut = { (e : UIMouseEvent) => () }

      override def canSelect = false
      def isExternal_=(b: Boolean): Unit = ()

      def optLabel: OptA[N] = None
      def optLabel_=(o: OptA[N]): Unit = ()

    }

    class NegativeBox[P <: Nat](
      val panel: CardinalPanel[S[P]]
    ) extends CardinalCellBox[S[P]] {

      val label: PolOptA[S[P]] = Negative()
      def label_=(po : PolOptA[S[P]]): Unit = ()
      val address: CardinalAddress[S[S[P]]] = null  // No address for the polarized cells ...
      val nestingAddress: Address[S[S[P]]] = List(List())
      val isExternal: Boolean = true
      val isPolarized: Boolean = true
      val visualization = panel.visualize(label)
      makeMouseInvisible(labelElement)

      boxRect.onClick = { (e : UIMouseEvent) => thisEditor.deselectAll }
      boxRect.onMouseOver = { (e : UIMouseEvent) => () }
      boxRect.onMouseOut = { (e : UIMouseEvent) => () }

      override def canSelect = false
      def isExternal_=(b: Boolean): Unit = ()

      def optLabel: OptA[S[P]] = None
      def optLabel_=(o: OptA[S[P]]): Unit = ()

    }

    //============================================================================================
    // PANEL IMPLEMENTATION
    //

    trait CardinalPanel[N <: Nat] extends ActivePanel[PolOptA[N], N] with GalleryPanel[N] {

      type BoxType = CardinalCellBox[N]
      type EdgeType = CardinalCellEdge[N]

      var cardinalBoxNesting: CardinalNesting[NeutralCellBox[N], N]

      def cardinalNesting: CardinalNesting[OptA[N], N] = 
        cardinalBoxNesting map {
          case nst => nst map (_.optLabel)
        }

      val positiveBox : CardinalCellBox[N] 
      val negativeBox : CardinalCellBox[N] 

      def nesting: Nesting[PolOptA[N], N] = 
        Cardinal.toPolarityNesting(panelDim)(cardinalNesting)

      def boxNesting: Nesting[CardinalCellBox[N], N] = 
        Cardinal.toNesting(panelDim)(cardinalBoxNesting, negativeBox, positiveBox)

      val config = thisEditor.config.panelConfig

      def visualize(a: PolOptA[N]) : Visualization[N] = {
        implicit val bnds = thisEditor.config.spacerBounds
        implicitly[VisualizableFamily[PolOptA]].visualize(panelDim)(a)
      }

      def seekToAddress(addr: CardinalAddress[S[N]]) : ShapeM[NestingZipper[BoxType, N]] = 
        for {
          pr <- Cardinal.poke(panelDim)(cardinalBoxNesting, addr.tail)
          z <- pr._1.seekTo(addr.head)
        } yield z

      def neutralCellBox(lbl: OptA[N], addr: CardinalAddress[S[N]], isExt: Boolean) : NeutralCellBox[N] = 
        new NeutralCellBox(this, lbl, addr, isExt)

      def cellEdge : CardinalCellEdge[N] = 
        new CardinalCellEdge(this)

      def generateNestingData(nst: Nesting[OptA[N], N], pref: CardinalAddress[N]): Nesting[NeutralCellBox[N], N]
      def generateBoxes(cn: CardinalNesting[OptA[N], N]) : CardinalNesting[NeutralCellBox[N], N] = 
        Cardinal.mapCardinalTreeWithAddr(panelDim)(cn)({
          case (nst, pref) => generateNestingData(nst, pref)
        })

      def refreshAddresses : Unit = {

        import scalaz.Id._

        Cardinal.traverseCardinalTreeWithAddr(panelDim)(cardinalBoxNesting)({
          case (nst, pref) => nst.traverseWithAddress[Id, Unit]({
            case (box, addr) => box.address = pref >> addr
          })
        })

      }

    }

    class CardinalObjectPanel(cn: CardinalNesting[OptA[_0], _0]) 
        extends CardinalPanel[_0] with ActiveObjectPanel[PolOptA[_0]] {

      def panelDim = Z

      var cardinalBoxNesting: CardinalNesting[NeutralCellBox[_0], _0] = 
        generateBoxes(cn)

      val positiveBox : CardinalCellBox[_0] = new PositiveBox(this)
      val negativeBox : CardinalCellBox[_0] = new PositiveBox(this)

      def generateNestingData(nst: Nesting[OptA[_0], _0], pref: CardinalAddress[_0]): Nesting[NeutralCellBox[_0], _0] = 
        Nesting.elimWithAddress[OptA[_0], Nesting[NeutralCellBox[_0], _0], _0](panelDim)(nst)({
          case (opt, addr) => Nesting.external(panelDim)(neutralCellBox(opt, pref >> addr, true))
        })({
          case (opt, addr, cn) => Box(neutralCellBox(opt, pref >> addr, false), cn)
        })

      refresh

    }

    class CardinalNestingPanel[P <: Nat](p: P)(
      cn: CardinalNesting[OptA[S[P]], S[P]],
      prevPanel: CardinalPanel[P]
    ) extends CardinalPanel[S[P]] with ActiveNestingPanel[PolOptA[S[P]], P] {

      def panelDim = S(p)

      var cardinalBoxNesting: CardinalNesting[NeutralCellBox[S[P]], S[P]] = 
        generateBoxes(cn)

      val positiveBox : CardinalCellBox[S[P]] = new PositiveBox(this)
      val negativeBox : CardinalCellBox[S[P]] = new NegativeBox(this)

      def generateNestingData(nst: Nesting[OptA[S[P]], S[P]], pref: CardinalAddress[S[P]]): Nesting[NeutralCellBox[S[P]], S[P]] =
        Nesting.elimWithAddress[OptA[S[P]], Nesting[NeutralCellBox[S[P]], S[P]], S[P]](panelDim)(nst)({
          case (opt, addr) => Nesting.external(panelDim)(neutralCellBox(opt, pref >> addr, true))
        })({
          case (opt, addr, cn) => Box(neutralCellBox(opt, pref >> addr, false), cn)
        })

      // This will need to be updated on a refresh
      var edgeNesting : Nesting[CardinalCellEdge[S[P]], P] = null

      override def refresh : Unit = {
        edgeNesting = connectEdges(prevPanel.boxNesting map (_ => cellEdge), boxNesting)
        super.refresh
      }

      refresh

    }

  }

  object CardinalEditor {

    def apply[A[_ <: Nat]](implicit v: VisualizableFamily[A]) : CardinalEditor[A] = {
      type OptA[K <: Nat] = Option[A[K]]
      new CardinalEditor[A](toFiniteCardinal[OptA, _0](Cardinal[OptA] >> Pt(Obj(None))))
    }

    def apply[A[_ <: Nat], N <: Nat](
      c: Complex[Lambda[`K <: Nat` => Option[A[K]]], N]
    )(implicit v: VisualizableFamily[A]) : CardinalEditor[A] = {
      type OptA[K <: Nat] = Option[A[K]]
      new CardinalEditor[A](toFiniteCardinal[OptA, N](Cardinal.complexToCardinal[OptA, N](c.length.pred)(c)._1))
    }

  }

}
