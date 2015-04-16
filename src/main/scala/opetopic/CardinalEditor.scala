/**
  * CardinalEditor.scala - A Base Class for Cardinal-Type Editors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds
import scala.collection.mutable.ListBuffer

import scalaz.syntax.monad._

import TypeDefs._
import Cardinal._

import syntax.tree._
import syntax.complex._
import syntax.nesting._
import syntax.cardinal._

abstract class CardinalEditor[A[_ <: Nat], U] extends Viewer[U] { thisEditor : Renderer[U] => 

  type LabelType[N <: Nat] <: Polarity[Option[A[N]]]
  type MarkerType[N <: Nat] <: CardinalMarker[N]

  var editorState : EditorState

  trait EditorState { thisState => 

    type Dim <: Nat

    val dim : Dim

    val cardinal : Cardinal[NeutralMarker, Dim]
    val polarizedMarkers : PolaritySuite[PolarizedMarker, Dim]

    val canvases : List[CardinalCanvas]
    val nextCanvas : CardinalCanvas

    def complex : Complex[CardinalMarker, Dim] = 
      cardinal.toComplexWith(polarizedMarkers)

  }

  object EditorState {

    type EditorStateAux[N <: Nat] = EditorState { type Dim = N }

    def apply(nst : Nesting[A[_0], _0]) : EditorStateAux[_0] = {

      val objCanvas = createCanvas
      val edgeCanvas = createCanvas

      val posMarker : PolarizedMarker[_0] = 
        createPositiveMarker[_0](objCanvas, edgeCanvas)

      def genObjData(nst: Nesting[A[_0], _0], base: Address[_1]) : Nesting[NeutralMarker[_0], _0] = 
        nst match {
          case Obj(a) => 
            Obj(createNeutralMarker(a, CardinalAddress() >> base, true, objCanvas, edgeCanvas))
          case Box(a, Pt(n)) =>
            Box(createNeutralMarker(a, CardinalAddress() >> base, false, objCanvas, edgeCanvas), 
              Pt(genObjData(n, () :: base)))
        }

      new EditorState {

        type Dim = _0

        val dim : Dim = Z

        val cardinal : Cardinal[NeutralMarker, Dim] = 
          Cardinal[NeutralMarker]() >> Pt(genObjData(nst, Nil))

        val polarizedMarkers : PolaritySuite[PolarizedMarker, Dim] = 
          PolaritySuite[PolarizedMarker]() >> (posMarker, posMarker)

        val canvases : List[CardinalCanvas] = List(objCanvas)
        val nextCanvas : CardinalCanvas = edgeCanvas

      }

    }

    def apply[N <: Nat](es: EditorStateAux[N], cn: CardinalNesting[A[S[N]], S[N]]) : EditorStateAux[S[N]] = {

      val nextDim = S(es.dim)

      val objCanvas = es.nextCanvas
      val edgeCanvas = createCanvas

      val posMarker : PolarizedMarker[S[N]] = createPositiveMarker[S[N]](objCanvas, edgeCanvas)
      val negMarker : PolarizedMarker[S[N]] = createNegativeMarker[S[N]](objCanvas, edgeCanvas)

      // I think this will be taken care of below ...
      // negMarker.outgoingEdgeMarker = 
      //   Some(PolaritySuite.head(es.polarizedMarkers)._2)

      def genNstData(nst: Nesting[A[S[N]], S[N]], pref: CardinalAddress[S[N]], base: Address[S[S[N]]]) : Nesting[NeutralMarker[S[N]], S[N]] = 
        nst match {
          case Dot(a, d) => 
            Dot(createNeutralMarker(a, pref >> base, true, objCanvas, edgeCanvas), d)
          case Box(a, cn) => 
            Box(createNeutralMarker(a, pref >> base, false, objCanvas, edgeCanvas), 
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

          val cardinal : Cardinal[NeutralMarker, Dim] =
            es.cardinal >> neutralNesting

          val polarizedMarkers : PolaritySuite[PolarizedMarker, Dim] =
            es.polarizedMarkers >> (negMarker, posMarker)

          val canvases : List[CardinalCanvas] =
            es.canvases :+ objCanvas

          val nextCanvas : CardinalCanvas =
            edgeCanvas

        }

      // So here we simply perform the inductive step of comultiplication
      // in the cardinal ....

      val newComplex : Complex[CardinalMarker, S[es.Dim]] = newState.complex

      // neutralBox traverseWithAddress {
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
  // CARDINAL CANVAS IMPLEMENTATION
  //

  abstract class CardinalCanvas extends ViewerCanvas

  def createCanvas : CardinalCanvas

  //============================================================================================
  // CARDINAL MARKERS
  //

  abstract class CardinalMarker[N <: Nat] extends ViewerMarker[N] {

    var faceComplex : Option[Complex[CardinalMarker, N]]

  }

  abstract class NeutralMarker[N <: Nat] extends CardinalMarker[N] {

    def element : A[N]
    def address : Address[S[N]]

  }

  abstract class PolarizedMarker[N <: Nat] extends CardinalMarker[N]

  def createNeutralMarker[N <: Nat](
    a: A[N], addr: CardinalAddress[S[N]], 
    isExternal: Boolean,
    objCanvas: CardinalCanvas, 
    edgeCanvas: CardinalCanvas
  ) : NeutralMarker[N]

  def createPositiveMarker[N <: Nat](
    objCanvas: CardinalCanvas, 
    edgeCanvas: CardinalCanvas
  ) : PolarizedMarker[N]

  def createNegativeMarker[N <: Nat](
    objCanvas: CardinalCanvas, 
    edgeCanvas: CardinalCanvas
  ) : PolarizedMarker[N]

  //============================================================================================
  // INITIALIZATION
  //

  def initializeEditor[N <: Nat](cardinal : Cardinal[A, N]) : EditorState.EditorStateAux[N] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = Cardinal[A, N] => EditorState.EditorStateAux[N]

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
