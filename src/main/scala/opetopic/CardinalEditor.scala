/**
  * CardinalEditor.scala - A Base Class for Cardinal-Type Editors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds
import scala.collection.mutable.ListBuffer

import TypeDefs._
import Cardinal._

abstract class CardinalEditor[M[+_], A[_ <: Nat], U] extends Viewer[M, U] { thisEditor : Renderer[M, U] => 

  type LabelType[N <: Nat] <: Polarity[Option[A[N]]]
  type MarkerType[N <: Nat] <: CardinalMarker[N]

  type PolarizedPair[N <: Nat] = 
    (PolarizedMarker[N], PolarizedMarker[N])

  var editorState : EditorState

  type EditorStateAux[N <: Nat] = EditorState { type Dim = N }

  trait EditorState { thisState => 

    type Dim <: Nat

    val cardinal : Cardinal[CardinalMarker, Dim]
    val canvases : List[CardinalCanvas]
    val polarizedMarkers : Suite[PolarizedPair, Dim]

    // implicit object MarkerGenerator extends CardinalCellGenerator[ConstMarker, NeutralMarker] {
    //   def positive[N <: Nat](n : N) : CardinalMarker = polarizedMarkers(natToInt(n))._1
    //   def negative[N <: Nat](n : N) : CardinalMarker = polarizedMarkers(natToInt(n))._2
    //   def neutral[N <: Nat](m : NeutralMarker) : CardinalMarker = m
    // }

    // Okay, things are starting to look kind of right.  The point is going to be that this
    // guy controls the linkages between the dimensions as he is build and that we just instantiate
    // new versions by calling with the next entry in the cardinal, spawning the generation routine.

    // Anyway, this will act as a kind of control object, and is probably what you have in 
    // mind for the stateful cardinal editor monad that you've had in mind.

    def extendWith(cn: CardinalNesting[A[S[Dim]], S[Dim]]) : EditorStateAux[S[Dim]] = {

      // Generate some polarized markers and whatnot and attach them to the state ....

      // new EditorState {

      //   type Dim = S[thisState.Dim]

      //   val cardinal = thisState.cardinal >> cn

      // }
      ???
    }

  }

  object EditorState {

    def apply(nst : Nesting[A[_0], _0]) : EditorStateAux[_0] = ???

  }

  //============================================================================================
  // CARDINAL CANVAS IMPLEMENTATION
  //

  abstract class CardinalCanvas extends ViewerCanvas

  //============================================================================================
  // CARDINAL MARKERS
  //

  abstract class CardinalMarker[N <: Nat] extends ViewerMarker[N]

  abstract class NeutralMarker[N <: Nat] extends CardinalMarker[N] {

    def element : A[N]
    def address : Address[S[N]]

  }

  abstract class PolarizedMarker[N <: Nat] extends CardinalMarker[N]

  //============================================================================================
  // INITIALIZATION
  //

  def initializeEditor[N <: Nat](cardinal : Cardinal[A, N]) : EditorStateAux[N] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = Cardinal[A, N] => EditorStateAux[N]

      def caseZero : Out[_0] = {
        case Cardinal(_, Pt(nst)) => 
          EditorState(nst)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case Cardinal(tl, hd) => 
          initializeEditor(tl).extendWith(hd)
      }

    })(cardinal.length.pred)(cardinal)

}
