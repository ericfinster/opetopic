/**
  * ComplexViewer.scala - A Viewer for Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scala.language.higherKinds

import scalaz.Id._

import opetopic._
import TypeDefs._

import syntax.tree._
import syntax.complex._
import syntax.nesting._

trait ComplexViewer[A[_ <: Nat], U] extends Viewer[A, U] {

  def createMarker[N <: Nat](n: N)(
    label: A[N], 
    addr: Address[S[N]],
    isExternal: Boolean,
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : MarkerType[N]

  //============================================================================================
  // INITIALIZATION
  //

  def initialize[N <: Nat](cmplx: Complex[A, N]) : (Complex[MarkerType, N], CanvasType) = 
    initialize(cmplx.length.pred)(cmplx)

  @natElim
  def initialize[N <: Nat](n: N)(cmplx: Complex[A, N]) : (Complex[MarkerType, N], CanvasType) = {
    case (Z, Complex(_, hd)) => {

      val objCanvas = createCanvas
      val edgeCanvas = createCanvas

      displayCanvas(objCanvas)

      def genObjData(
        nst: Nesting[A[_0], _0],
        base: Address[_1],
        objCanvas: CanvasType,
        edgeCanvas: CanvasType
      ) : Nesting[MarkerType[_0], _0] =
        nst match {
          case Obj(a) => {
            val marker = createMarker(Z)(a, base, true, objCanvas, edgeCanvas)
            marker.faceComplex = Some(Complex() >> Obj(marker))
            Obj(marker)
          }
          case Box(a, Pt(n)) => {
            val marker = createMarker(Z)(a, base, false, objCanvas, edgeCanvas)
            marker.faceComplex = Some(Complex() >> Obj(marker))
            Box(marker, Pt(genObjData(n, () :: base, objCanvas, edgeCanvas)))
          }
        }

      (Complex[MarkerType]() >> genObjData(hd, Nil, objCanvas, edgeCanvas), edgeCanvas)

    }
    case (S(p), Complex(tl, hd)) => {

      val (newTl, objCanvas) = initialize(p)(tl)
      val edgeCanvas = createCanvas

      displayCanvas(objCanvas)

      def genNestingData[N <: Nat](n: N)(
        nst: Nesting[A[S[N]], S[N]],
        base: Address[S[S[N]]],
        objCanvas: CanvasType,
        edgeCanvas: CanvasType
      ) : Nesting[MarkerType[S[N]], S[N]] =
        nst match {
          case Dot(a, d) => {
            val marker = createMarker(S(n))(a, base, true, objCanvas, edgeCanvas)
            Dot(marker, d)
          }
          case Box(a, cn) => {
            val marker = createMarker(S(n))(a, base, false, objCanvas, edgeCanvas)
            Box(marker, cn mapWithAddress {
              (nt, addr) => genNestingData(n)(nt, addr :: base, objCanvas, edgeCanvas)
            })
          }
        }

      val newHd = genNestingData(p)(hd, Nil, objCanvas, edgeCanvas)
      val markerComplex = newTl >> newHd

      newHd traverseWithAddress[Id, Unit] {
        case (mk, addr) => for {
          src <- markerComplex.sourceAt(addr)
        } yield {
          mk.faceComplex = Some(src)
        }
      }

      newHd match {
        case Dot(mk, d) => {
          mk.outgoingEdgeMarker = Some(newTl.head.baseValue)
          mk.faceComplex = Some(newTl >> newHd)
        }
        case Box(mk, cn) => {
          for {
            dots <- Nesting.spineFromCanopy(cn)
            edges = newTl.head.toTree
            _ <- dots.mapWith(edges)({
              case (dotMarker, edgeMarker) => {
                dotMarker.outgoingEdgeMarker = Some(edgeMarker)
              }
            })
          } yield ()
        }
      }

      (markerComplex, edgeCanvas)

    }
  }
}
