/**
  * CardinalEditor.scala - A Base Class for Cardinal-Type Editors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scala.language.higherKinds
import scala.collection.mutable.ListBuffer

import scalaz.Applicative
import scalaz.syntax.monad._

import opetopic._
import TypeDefs._
import Cardinal._

import syntax.tree._
import syntax.complex._
import syntax.nesting._
import syntax.cardinal._

trait CardinalEditor[A[_ <: Nat], U] extends Viewer[({ type L[K <: Nat] = Polarity[Option[A[K]]] })#L, U] {

  type PolA[K <: Nat] = Polarity[Option[A[K]]]
  type OptA[K <: Nat] = Option[A[K]]

  type MarkerType[N <: Nat] <: CardinalMarker[N]
  type NeutralMarkerType[N <: Nat] <: MarkerType[N] with NeutralMarker[N]
  type PolarizedMarkerType[N <: Nat] <: MarkerType[N] with PolarizedMarker[N]

  def complex : FiniteComplex[MarkerType] = 
    complexToFiniteComplex(editorState.complex)

  //============================================================================================
  // EDITOR STATE
  //

  var editorState : EditorState

  trait EditorState { thisState => 

    type Dim <: Nat

    val dim : Dim

    val complex : Complex[MarkerType, Dim]
    val cardinal : Cardinal[NeutralMarkerType, Dim]

    val polarizedMarkers : PolaritySuite[PolarizedMarkerType, Dim]

    val canvases : List[CanvasType]
    val nextCanvas : CanvasType

    def refreshCardinalAddresses : Unit = {
      type T[K <: Nat] = CardinalNesting[NeutralMarkerType[K], K]

      Suite.foreach[T, S[Dim]](cardinal)(new IndexedOp[T] {
        def apply[N <: Nat](n: N)(cn: T[N]) : Unit = {
          implicit val curDim = n

          import scalaz.Id._

          traverseCardinalTreeWithAddr(n)(cn)({ 
            case (nst, base) => nst traverseWithAddress[Id, Unit] {
              case (mk, addr) => {
                mk.cardinalAddress = Some(base >> addr)
              }
            }
          })
        }
      })

    }

    def refreshComplexAddresses : Unit = {
      type T[K <: Nat] = Nesting[MarkerType[K], K]

      Suite.foreach[T, S[Dim]](complex)(new IndexedOp[T] {
        def apply[N <: Nat](n: N)(nst: T[N]) : Unit = {

          import scalaz.Id._

          nst traverseWithAddress[Id, Unit] {
            case (mk, addr) => {
              mk.nestingAddress = addr
            }
          }
        }
      })
    }

    def refreshFaceComplexes : Unit = {
      type MarkerComplex[K <: Nat] = Complex[MarkerType, K]
      for {
        dblcmplx <- complex.comultiply
      } {
        dblcmplx.foreach(new IndexedOp[MarkerComplex] {
          def apply[N <: Nat](n: N)(mc: MarkerComplex[N]) : Unit = {
            mc.head.baseValue.faceComplex = Some(mc)
          }
        })
      }
    }

  }

  object EditorState {

    type EditorStateAux[N <: Nat] = EditorState { type Dim = N }

    def apply(nst : Nesting[Option[A[_0]], _0]) : EditorStateAux[_0] = {

      val objCanvas = createCanvas
      val edgeCanvas = createCanvas

      displayCanvas(objCanvas)

      val posMarker : PolarizedMarkerType[_0] = 
        createPositiveMarker(Z)(objCanvas, edgeCanvas)

      def genObjData(nst: Nesting[Option[A[_0]], _0], base: Address[_1]) : Nesting[NeutralMarkerType[_0], _0] = 
        nst match {
          case Obj(opt) => {
            val marker = createNeutralMarker(Z)(opt, CardinalAddress() >> base, true, objCanvas, edgeCanvas)
            marker.faceComplex = Some(Complex() >> Obj(marker))
            Obj(marker)
          }
          case Box(opt, Pt(n)) => {
            val canopy = genObjData(n, () :: base)
            val marker = createNeutralMarker(Z)(opt, CardinalAddress() >> base, false, objCanvas, edgeCanvas)
            marker.faceComplex = Some(Complex() >> Obj(marker))
            Box(marker, Pt(canopy))
          }
        }

      val neutralCanopy = Pt(genObjData(nst, Nil))

      new EditorState {

        type Dim = _0

        val dim : Dim = Z

        val complex : Complex[MarkerType, Dim] = 
          Complex[MarkerType]() >> Box(posMarker, neutralCanopy)

        val cardinal : Cardinal[NeutralMarkerType, Dim] = 
          Cardinal[NeutralMarkerType]() >> neutralCanopy

        val polarizedMarkers : PolaritySuite[PolarizedMarkerType, Dim] = 
          PolaritySuite[PolarizedMarkerType]() >> (posMarker, posMarker)

        val canvases : List[CanvasType] = List(objCanvas)
        val nextCanvas : CanvasType = edgeCanvas

      }

    }

    def apply[N <: Nat](es: EditorStateAux[N], cn: CardinalNesting[Option[A[S[N]]], S[N]]) : EditorStateAux[S[N]] = {

      val nextDim = S(es.dim)

      val objCanvas = es.nextCanvas
      val edgeCanvas = createCanvas

      displayCanvas(objCanvas)

      val posMarker : PolarizedMarkerType[S[N]] = createPositiveMarker(nextDim)(objCanvas, edgeCanvas)
      val negMarker : PolarizedMarkerType[S[N]] = createNegativeMarker(nextDim)(objCanvas, edgeCanvas)

      def genNstData(nst: Nesting[Option[A[S[N]]], S[N]], pref: CardinalAddress[S[N]], base: Address[S[S[N]]]) 
          : Nesting[NeutralMarkerType[S[N]], S[N]] =
        nst match {
          case Dot(opt, d) => 
            Dot(createNeutralMarker(nextDim)(opt, pref >> base, true, objCanvas, edgeCanvas), d)
          case Box(opt, cn) => {
            val newCanopy = cn mapWithAddress {
                case (nst, dir) => genNstData(nst, pref, dir :: base)
              }

            Box(createNeutralMarker(nextDim)(opt, pref >> base, false, objCanvas, edgeCanvas), newCanopy)
          }
        }

      val neutralNesting = mapCardinalTreeWithAddr(nextDim)(cn)({
        case (nst, pref) => genNstData(nst, pref, Nil)
      })

      val neutralCanopy = Node(Dot(negMarker, nextDim), toShell(es.dim)(neutralNesting))
      val neutralBox = Box(posMarker, neutralCanopy)

      for {
        dots <- Nesting.spineFromCanopy(neutralCanopy)
        edges = es.complex.head.toTree
        _ <- dots.mapWith(edges)({
          case (dotMarker, edgeMarker) => {
            dotMarker.outgoingEdgeMarker = Some(edgeMarker)
          }
        })
      } yield ()

      val extendedComplex = es.complex >> neutralBox

      neutralBox traverseWithAddress[ShapeM, Unit] {
        case (mk, addr) => 
          for {
            fc <- extendedComplex sourceAt addr
          } yield {
            mk.nestingAddress = addr
            mk.faceComplex = Some(fc)
          }
      }

      new EditorState {

        type Dim = S[es.Dim]

        val dim = nextDim

        val complex : Complex[MarkerType, Dim] = 
          extendedComplex

        val cardinal : Cardinal[NeutralMarkerType, Dim] =
          es.cardinal >> neutralNesting

        val polarizedMarkers : PolaritySuite[PolarizedMarkerType, Dim] =
          es.polarizedMarkers >> (negMarker, posMarker)

        val canvases : List[CanvasType] =
          es.canvases :+ objCanvas

        val nextCanvas : CanvasType =
          edgeCanvas

      }

    }

  }

  //============================================================================================
  // CARDINAL MARKERS
  //

  trait CardinalMarker[N <: Nat] extends ViewerMarker[N] {

    def isExtrudable : Boolean
    var cardinalAddress : Option[CardinalAddress[S[N]]] = None

  }

  trait NeutralMarker[N <: Nat] extends CardinalMarker[N] {

    def isExtrudable : Boolean = 
      cardinalAddress match {
        case Some(_ >> Nil) => true
        case _ => false
      }

    def neutralComplex : Option[Complex[OptA, N]] = {
      import scalaz.std.option._
      for {
        fc <- faceComplex
        res <- fc.traverse(new IndexedTraverse[Option, MarkerType, OptA] {
          def apply[N <: Nat](n: N)(mk: MarkerType[N]) : Option[OptA[N]] = 
            mk.label match {
              case Neutral(optA) => Some(optA)
              case _ => None
            }
        })
      } yield res
    }

  }

  trait PolarizedMarker[N <: Nat] extends CardinalMarker[N] {

    def isExtrudable : Boolean = false

    override def hover = ()
    override def unhover = ()
    override def select = ()
    override def deselect = ()

  }

  def createNeutralMarker[N <: Nat](n: N)(
    opt: Option[A[N]], 
    addr: CardinalAddress[S[N]],
    isExternal: Boolean,
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : NeutralMarkerType[N]

  def createPositiveMarker[N <: Nat](n: N)(
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : PolarizedMarkerType[N]

  def createNegativeMarker[N <: Nat](n: N)(
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : PolarizedMarkerType[N]

  //============================================================================================
  // INITIALIZATION
  //

  def initializeEditor[N <: Nat](cardinal : Cardinal[OptA, N]) : EditorState.EditorStateAux[N] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = Cardinal[OptA, N] => EditorState.EditorStateAux[N]

      def caseZero : Out[_0] = {
        case Cardinal(_, Pt(nst)) => 
          EditorState(nst)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case Cardinal(tl, hd) => 
          EditorState(initializeEditor(tl), hd)
      }

    })(cardinal.length.pred)(cardinal)

  //============================================================================================
  // EXTRUSION GARBAGE
  //

  def extendedState : EditorState = {

    val curState = editorState

    type D = curState.Dim  
    implicit val d = curState.dim

    val nextCard : CardinalNesting[Option[A[S[D]]], S[D]] = 
      curState.cardinal.head map {
        case nst => Nesting.extendNesting(nst)(_ => None)
      }

    EditorState[D](curState, nextCard)

  }

  def extend : Unit = {
    editorState = extendedState
    render
  }

  def extrudeSelection : Unit = {

    val curState = editorState

    type D = curState.Dim
    implicit val d = curState.dim

    selection match {
      case None => ()
      case Some(sel) =>
        if (sel.root.isExtrudable) {

          val selectionDim : Int = natToInt(sel.dim)
          val curStateDim : Int = natToInt(curState.dim)

          val (extrusionState, fillerEdgeCanvas) = 
            if (selectionDim == curStateDim) {
              val newState = extendedState
              (newState, newState.nextCanvas)
            } else if (selectionDim == (curStateDim - 1)) {
              (curState, curState.nextCanvas)
            } else (curState, curState.canvases(selectionDim + 2))

          val targetCanvas = extrusionState.canvases(natToInt(sel.dim))
          val fillerCanvas = extrusionState.canvases(natToInt(sel.dim) + 1)

          for {
            diff <- fromOpt(Lte.diffOpt(sel.dim, extrusionState.dim))
            ca <- fromOpt(sel.root.cardinalAddress)
            mk0 = createNeutralMarker(sel.dim)(None, ca, false, targetCanvas, fillerCanvas)
            mk1 = createNeutralMarker(S(sel.dim))(None, ca >> Nil, true, fillerCanvas, fillerEdgeCanvas)
            _ = mk1.outgoingEdgeMarker = Some(mk0) // This should be the only change to these, no?
            newCardinal <- Cardinal.extrudeSelection(extrusionState.cardinal, Suite.tail(ca), mk0, mk1)(
              mk => mk.isSelected
            )(diff)
          } yield {

            deselectAll

            val newComplex : Complex[MarkerType, extrusionState.Dim] = 
              completeToComplex(extrusionState.dim)(newCardinal, extrusionState.polarizedMarkers)

            editorState = new EditorState {

              type Dim = extrusionState.Dim
              val dim = extrusionState.dim
              val complex = newComplex
              val cardinal = newCardinal
              val polarizedMarkers = extrusionState.polarizedMarkers
              val canvases = extrusionState.canvases
              val nextCanvas = extrusionState.nextCanvas

            }

            editorState.refreshCardinalAddresses
            editorState.refreshComplexAddresses
            editorState.refreshFaceComplexes

            render

            selectAsRoot(mk0)

          }
        }
    }

  }

  def extrudeDrop : Unit = {

    val curState = editorState

    type D = curState.Dim
    implicit val d = curState.dim

    selection match {
      case None => ()
      case Some(sel) =>
        if (sel.root.isExtrudable) {

          val selectionDim : Int = natToInt(sel.dim)
          val curStateDim : Int = natToInt(curState.dim)

          val (extrusionState, fillerEdgeCanvas) = 
            if (selectionDim == curStateDim) {
              editorState = extendedState
              val newState = extendedState
              (newState, newState.nextCanvas)
            } else if (selectionDim == (curStateDim - 1)) {
              val newState = extendedState
              (newState, newState.nextCanvas)
            } else if (selectionDim == (curStateDim - 2)) {
              (curState, curState.nextCanvas)
            } else (curState, curState.canvases(selectionDim + 3))

          val targetCanvas = extrusionState.canvases(natToInt(sel.dim) + 1)
          val fillerCanvas = extrusionState.canvases(natToInt(sel.dim) + 2)

          for {
            diff <- fromOpt(Lte.diffOpt(sel.dim, extrusionState.dim))
            ca <- fromOpt(sel.root.cardinalAddress)
            mk0 = createNeutralMarker(S(sel.dim))(None, ca >> Nil, false, targetCanvas, fillerCanvas)
            mk1 = createNeutralMarker(S(S(sel.dim)))(None, ca >> Nil >> Nil, true, fillerCanvas, fillerEdgeCanvas)
            _ = mk1.outgoingEdgeMarker = Some(mk0) 
            newCardinal <- Cardinal.dropAtAddress(extrusionState.cardinal, Suite.tail(ca), mk0, mk1)(diff)
          } yield {

            deselectAll

            val newComplex : Complex[MarkerType, extrusionState.Dim] = 
              completeToComplex(extrusionState.dim)(newCardinal, extrusionState.polarizedMarkers)

            editorState = new EditorState {

              type Dim = extrusionState.Dim
              val dim = extrusionState.dim
              val complex = newComplex
              val cardinal = newCardinal
              val polarizedMarkers = extrusionState.polarizedMarkers
              val canvases = extrusionState.canvases
              val nextCanvas = extrusionState.nextCanvas

            }

            editorState.refreshCardinalAddresses
            editorState.refreshComplexAddresses
            editorState.refreshFaceComplexes

            render

            selectAsRoot(sel.root)

          }
        }
    }

  }

}
