/**
  * CardinalEditor.scala - Cardinal Editor Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import syntax.suite._
import syntax.nesting._
import syntax.complex._
import syntax.cardinal._
import TypeLemmas._

trait HasEditor extends { self: ActiveFramework with HasActivePanels with HasSelectableGalleries => 

  import isNumeric._

  class CardinalEditor[A[_ <: Nat], E <: Element](c: FiniteCardinal[Lambda[`N <: Nat` => Option[A[N]]]])(
    implicit val config: GalleryConfig, val r: AffixableFamily[A, E]
  ) extends SelectableGallery[Lambda[`N <: Nat` => Polarity[Option[A[N]]]], Element] { thisEditor =>

    type OptA[N <: Nat] = Option[A[N]]
    type PolOptA[N <: Nat] = Polarity[Option[A[N]]]

    type PanelType[N <: Nat] = CardinalPanel[N]
    type PanelSuite[N <: Nat] = Suite[PanelType, S[N]]
    type PanelAddressType[N <: Nat] = CardinalAddress[S[N]]
    type EditorStateAux[N <: Nat] = EditorState { type Dim = N }
    type NeutralBoxType[N <: Nat] = CardinalPanel[N]#NeutralCellBox

    trait EditorState {

      type Dim <: Nat
      val dim : Dim

      val cardinal : Cardinal[OptA, Dim]
      val panels: PanelSuite[Dim]

    }

    object EditorState {

      def apply[N <: Nat](c: Cardinal[OptA, N], ps: PanelSuite[N]) : EditorState = 
        new EditorState {
          type Dim = N
          val dim = c.length.pred
          val cardinal = c
          val panels = ps
        }

    }

    var editorState : EditorState = 
      EditorState[c.N](c.value, createPanels(c.n)(c.value))

    def panels: NonemptySuite[CardinalPanel] = 
      editorState.panels

    var selection: Option[Selection] = None

    val galleryViewport = viewport
    def element: Element = galleryViewport
    var bounds = Bounds(zero, zero, zero, zero)

    def initialize : Unit = {
      val (panelEls, bnds) = elementsAndBounds
      galleryViewport.width = config.width
      galleryViewport.height = config.height
      galleryViewport.setBounds(bnds)
      galleryViewport.children = panelEls
      bounds = bnds
    }

    initialize

    @natElim
    def createPanels[N <: Nat](n: N)(cardinal: Cardinal[OptA, N]) : Suite[CardinalPanel, S[N]] = {
      case (Z, Cardinal(_, objNst)) => SNil[CardinalPanel] >> new CardinalObjectPanel(objNst)
      case (S(p: P), Cardinal(tl, hd)) => {
        val newTail = createPanels(p)(tl) 
        val newHead = new CardinalNestingPanel(p)(hd, newTail.head)
        newTail >> newHead
      }
    }

    def boxCardinal[N <: Nat](st: EditorStateAux[N]) : Cardinal[NeutralBoxType, N] = {

      type NestingType[K <: Nat] = CardinalNesting[NeutralBoxType[K], K]

      val ps = st.panels

      ps.map(
        new IndexedMap[PanelType, NestingType] {
          def apply[K <: Nat](k: K)(p: PanelType[K]) : NestingType[K] = 
            p.cardinalBoxNesting
        }
      )

    }

    //============================================================================================
    // MUTABILITY ROUTINES
    //

    def extendEditorState(st: EditorState) : EditorState = {

      implicit val n = st.dim

      val extendedNesting = st.panels.head.cardinalNesting map {
        case nst => Nesting.extendNesting(nst)(_ => None)
      }

      EditorState(
        st.cardinal >> extendedNesting,
        st.panels >> new CardinalNestingPanel(n)(extendedNesting, st.panels.head)
      )
    }

    def extrudeSelection : Unit = {

      println("Starting extrusion ...")

      selection match {
        case None => ()
        case Some(sel) =>
          if (! sel.root.isExtrudable) println("Selection not extrudable") else {

            val selectionDim : Int = natToInt(sel.dim)
            val currentDim : Int = natToInt(panels.p)

            val extrusionState = 
              if (selectionDim == currentDim) {
                extendEditorState(editorState)
              } else editorState

            val extrusionCardinal : Cardinal[NeutralBoxType, extrusionState.Dim] = 
              boxCardinal(extrusionState)

            val extrusionAddress : CardinalAddress[sel.Dim] = 
              Suite.tail[Address, S[sel.Dim]](sel.root.address)

            for {
              diff <- fromOpt(diffOpt(S(sel.dim), extrusionState.dim))
              tgtPanel <- fromOpt(extrusionState.panels.getOpt(sel.dim))
              fillPanel = extrusionState.panels.get(S(sel.dim))(diff)
              tgtBox = tgtPanel.neutralCellBox(None, sel.root.address, false)
              fillBox = fillPanel.neutralCellBox(None, sel.root.address >> Nil, true)
              extrudedCardinal <- extrusionCardinal.extrudeSelection(sel.dim)(
                extrusionAddress, tgtBox, fillBox
              )(box => box.isSelected)(diff)
            } yield {

              println("Extrusion successful!!!")

              type ResType[K <: Nat] = (CardinalNesting[NeutralBoxType[K], K], PanelType[K])
              type ResDim = S[extrusionState.Dim]

              val test : Unit = 
                Suite.foreach[ResType, ResDim](extrudedCardinal.zipWith(extrusionState.panels))(
                  new IndexedOp[ResType] {
                    def apply[N <: Nat](n: N)(r: ResType[N]) = {
                      val (nst, pn) = r
                      type LocalType[K <: Nat] = CardinalNesting[pn.NeutralCellBox, K]
                      pn.cardinalBoxNesting = nst.asInstanceOf[LocalType[N]] // AHHHHHH!!!
                    }
                  }
                )

              // Now, the last step is to rebuild what parts of the interface have been
              // broken by the extension ...

              // First I think we should zip the resulting cardinals with the panel and
              // then we should replace the cardinal nesting there with the new one, then
              // refresh, etc ...

              //         deselectAll

              //         editorState.refreshCardinalAddresses
              //         editorState.refreshComplexAddresses
              //         editorState.refreshFaceComplexes

              //         render

              //         selectAsRoot(mk0)

            }
          }
      }
    }


    //============================================================================================
    // PANEL IMPLEMENTATION
    //

    trait CardinalPanel[N <: Nat] extends ActivePanel[PolOptA[N], Element, N] with GalleryPanel[N] {

      type BoxType = CardinalCellBox
      type EdgeType = CardinalCellEdge

      var cardinalNesting: CardinalNesting[OptA[N], N]
      var cardinalBoxNesting: CardinalNesting[NeutralCellBox, N]

      def positiveBox : CardinalCellBox = PositiveBox
      def negativeBox : CardinalCellBox = PositiveBox

      def nesting: Nesting[PolOptA[N], N] = 
        Cardinal.toPolarityNesting(panelDim)(cardinalNesting)

      def boxNesting: Nesting[CardinalCellBox, N] = 
        Cardinal.toNesting(panelDim)(cardinalBoxNesting, negativeBox, positiveBox)

      val config = thisEditor.config.panelConfig

      val affixable : Affixable[PolOptA[N], Element] = 
        Affixable.polarityAffixable(
          Affixable.optionAffixable(
            r(panelDim)
          )
        )

      def seekToAddress(addr: CardinalAddress[S[N]]) : ShapeM[NestingZipper[BoxType, N]] = 
        for {
          pr <- Cardinal.poke(panelDim)(cardinalBoxNesting, addr.tail)
          z <- pr._1.seekTo(addr.head)
        } yield z

      def neutralCellBox(lbl: OptA[N], addr: CardinalAddress[S[N]], isExt: Boolean) : NeutralCellBox = 
        new NeutralCellBox(lbl, addr, isExt)

      def cellEdge : EdgeType = new CardinalCellEdge

      def generateNestingData(nst: Nesting[OptA[N], N], pref: CardinalAddress[N]): Nesting[NeutralCellBox, N]
      def generateBoxes(cn: CardinalNesting[OptA[N], N]) : CardinalNesting[NeutralCellBox, N] = 
        Cardinal.mapCardinalTreeWithAddr(panelDim)(cn)({
          case (nst, pref) => generateNestingData(nst, pref)
        })

      trait CardinalCellBox extends ActiveCellBox {

        def isPolarized : Boolean

        def isExtrudable : Boolean =
          address match {
            case (_ >> Nil) => ! isPolarized
            case _ => false
          }

      }

      class CardinalCellEdge extends ActiveCellEdge

      class NeutralCellBox(val optLabel: OptA[N], val address: CardinalAddress[S[N]], val isExternal: Boolean)  
          extends CardinalCellBox { thisBox =>

        def label: PolOptA[N] = Neutral(optLabel)
        val decoration = affixable.decoration(label)
        val isPolarized = false
        makeMouseInvisible(labelElement)

        boxRect.onClick = { (e: UIMouseEvent) => thisEditor.select(thisBox) }
        boxRect.onMouseOver = { (e : UIMouseEvent) => setHoveredStyle }
        boxRect.onMouseOut = { (e : UIMouseEvent) => setUnhoveredStyle }

        override def select = { isSelected = true ; setSelectedStyle }
        override def deselect = { isSelected = false ; setDeselectedStyle }

      }

      object PositiveBox extends CardinalCellBox {

        val label: PolOptA[N] = Positive()
        val address: CardinalAddress[S[N]] = null // No address for polarized cells
        val isExternal: Boolean = false
        val isPolarized: Boolean = true
        val decoration = affixable.decoration(label)
        makeMouseInvisible(labelElement)

        boxRect.onClick = { (e : UIMouseEvent) => thisEditor.deselectAll }
        boxRect.onMouseOver = { (e : UIMouseEvent) => () }
        boxRect.onMouseOut = { (e : UIMouseEvent) => () }

        override def canSelect = false
        override def colorHint = "lightgrey"

      }

    }

    class CardinalObjectPanel(cn: CardinalNesting[OptA[_0], _0]) 
        extends CardinalPanel[_0] with ActiveObjectPanel[PolOptA[_0], Element] {

      def panelDim = Z

      var cardinalNesting: CardinalNesting[OptA[_0], _0] = cn
      var cardinalBoxNesting: CardinalNesting[NeutralCellBox, _0] = generateBoxes(cn)

      def generateNestingData(nst: Nesting[OptA[_0], _0], pref: CardinalAddress[_0]): Nesting[NeutralCellBox, _0] = 
        Nesting.elimWithAddress[OptA[_0], Nesting[NeutralCellBox, _0], _0](panelDim)(nst)({
          case (opt, addr) => Nesting.external(panelDim)(neutralCellBox(opt, pref >> addr, true))
        })({
          case (opt, addr, cn) => Box(neutralCellBox(opt, pref >> addr, false), cn)
        })

      refresh

    }

    class CardinalNestingPanel[P <: Nat](p: P)(
      cn: CardinalNesting[OptA[S[P]], S[P]],
      prevPanel: CardinalPanel[P]
    ) extends CardinalPanel[S[P]] with ActiveNestingPanel[PolOptA[S[P]], Element, P] {

      def panelDim = S(p)

      var cardinalNesting: CardinalNesting[OptA[S[P]], S[P]] = cn
      var cardinalBoxNesting: CardinalNesting[NeutralCellBox, S[P]] = generateBoxes(cn)

      override def negativeBox = NegativeBox

      object NegativeBox extends CardinalCellBox {

        val label: PolOptA[S[P]] = Negative()
        val address: CardinalAddress[S[S[P]]] = null  // No address for the polarized cells ...
        val isExternal: Boolean = true
        val isPolarized: Boolean = true
        val decoration = affixable.decoration(label)
        makeMouseInvisible(labelElement)

        boxRect.onClick = { (e : UIMouseEvent) => thisEditor.deselectAll }
        boxRect.onMouseOver = { (e : UIMouseEvent) => () }
        boxRect.onMouseOut = { (e : UIMouseEvent) => () }

        override def canSelect = false
        override def colorHint = "lightgrey"

      }

      def generateNestingData(nst: Nesting[OptA[S[P]], S[P]], pref: CardinalAddress[S[P]]): Nesting[NeutralCellBox, S[P]] =
        Nesting.elimWithAddress[OptA[S[P]], Nesting[NeutralCellBox, S[P]], S[P]](panelDim)(nst)({
          case (opt, addr) => Nesting.external(panelDim)(neutralCellBox(opt, pref >> addr, true))
        })({
          case (opt, addr, cn) => Box(neutralCellBox(opt, pref >> addr, false), cn)
        })

      // This will need to be updated on a refresh
      var edgeNesting : Nesting[EdgeType, P] = 
        connectEdges(prevPanel.boxNesting map (_ => cellEdge), boxNesting)


      refresh

    }

  }

}
