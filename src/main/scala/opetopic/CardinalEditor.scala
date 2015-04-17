/**
  * CardinalEditor.scala - A Base Class for Cardinal-Type Editors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds
import scala.collection.mutable.ListBuffer

import scalaz.Applicative
import scalaz.syntax.monad._

import TypeDefs._
import Cardinal._

import syntax.tree._
import syntax.complex._
import syntax.nesting._
import syntax.cardinal._

trait CardinalEditor[A[_ <: Nat], U] extends Viewer[U] { 

  type OptA[K <: Nat] = Option[A[K]]

  type LabelType[N <: Nat] <: Polarity[Option[A[N]]]

  type MarkerType[N <: Nat] <: CardinalMarker[N]
  type NeutralMarkerType[N <: Nat] <: MarkerType[N] with NeutralMarker[N]
  type PolarizedMarkerType[N <: Nat] <: MarkerType[N] with PolarizedMarker[N]

  def render : Unit = {
    val es : EditorState = editorState
    renderComplex[es.Dim](es.complex)
  }

  //============================================================================================
  // EDITOR STATE
  //

  var editorState : EditorState

  trait EditorState { thisState => 

    type Dim <: Nat

    val dim : Dim

    val cardinal : Cardinal[NeutralMarkerType, Dim]
    val polarizedMarkers : PolaritySuite[PolarizedMarkerType, Dim]

    val canvases : List[CanvasType]
    val nextCanvas : CanvasType

    def complex : Complex[MarkerType, Dim] = 
      cardinal.toComplexWith(polarizedMarkers)

  }

  object EditorState {

    type EditorStateAux[N <: Nat] = EditorState { type Dim = N }

    def apply(nst : Nesting[Option[A[_0]], _0]) : EditorStateAux[_0] = {

      val objCanvas = createCanvas
      val edgeCanvas = createCanvas

      displayCanvas(objCanvas)

      val posMarker : PolarizedMarkerType[_0] = 
        createPositiveMarker[_0](objCanvas, edgeCanvas)

      def genObjData(nst: Nesting[Option[A[_0]], _0], base: Address[_1]) : Nesting[NeutralMarkerType[_0], _0] = 
        nst match {
          case Obj(opt) => 
            Obj(createNeutralMarker(opt, CardinalAddress() >> base, true, objCanvas, edgeCanvas))
          case Box(opt, Pt(n)) =>
            Box(createNeutralMarker(opt, CardinalAddress() >> base, false, objCanvas, edgeCanvas), 
              Pt(genObjData(n, () :: base)))
        }

      // Have to generate the object complexes as well ...

      new EditorState {

        type Dim = _0

        val dim : Dim = Z

        val cardinal : Cardinal[NeutralMarkerType, Dim] = 
          Cardinal[NeutralMarkerType]() >> Pt(genObjData(nst, Nil))

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

      val posMarker : PolarizedMarkerType[S[N]] = createPositiveMarker[S[N]](objCanvas, edgeCanvas)
      val negMarker : PolarizedMarkerType[S[N]] = createNegativeMarker[S[N]](objCanvas, edgeCanvas)

      // I think this will be taken care of below ...
      // negMarker.outgoingEdgeMarker = 
      //   Some(PolaritySuite.head(es.polarizedMarkers)._2)

      def genNstData(nst: Nesting[Option[A[S[N]]], S[N]], pref: CardinalAddress[S[N]], base: Address[S[S[N]]]) 
          : Nesting[NeutralMarkerType[S[N]], S[N]] =
        nst match {
          case Dot(opt, d) => 
            Dot(createNeutralMarker(opt, pref >> base, true, objCanvas, edgeCanvas), d)
          case Box(opt, cn) => 
            Box(createNeutralMarker(opt, pref >> base, false, objCanvas, edgeCanvas), 
              cn mapWithAddress {
                case (nst, dir) => genNstData(nst, pref, dir :: base)
              }
            )
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

      val newState = 
        new EditorState {

          type Dim = S[es.Dim]

          val dim = nextDim

          val cardinal : Cardinal[NeutralMarkerType, Dim] =
            es.cardinal >> neutralNesting

          val polarizedMarkers : PolaritySuite[PolarizedMarkerType, Dim] =
            es.polarizedMarkers >> (negMarker, posMarker)

          val canvases : List[CanvasType] =
            es.canvases :+ objCanvas

          val nextCanvas : CanvasType =
            edgeCanvas

        }

      // So here we simply perform the inductive step of comultiplication
      // in the cardinal ....

      // val newComplex : Complex[CardinalMarker, S[es.Dim]] = newState.complex

      // neutralBox traverseWithAddress[ShapeM, Unit] {
      //   case (mk, addr) => 
      //     for {
      //       fc <- newComplex sourceAt addr
      //     } yield {
      //       mk.faceComplex = Some(fc)
      //       ()
      //     }
      // }

      newState
    }

  }

  //============================================================================================
  // CARDINAL MARKERS
  //

  trait CardinalMarker[N <: Nat] extends ViewerMarker[N] 

  trait NeutralMarker[N <: Nat] extends CardinalMarker[N] {

    def element : Option[A[N]]
    def address : CardinalAddress[S[N]]

  }

  trait PolarizedMarker[N <: Nat] extends CardinalMarker[N]

  def createNeutralMarker[N <: Nat](
    opt: Option[A[N]], 
    addr: CardinalAddress[S[N]],
    isExternal: Boolean,
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : NeutralMarkerType[N]

  def createPositiveMarker[N <: Nat](
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : PolarizedMarkerType[N]

  def createNegativeMarker[N <: Nat](
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

}
