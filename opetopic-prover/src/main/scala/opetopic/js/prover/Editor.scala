/**
  * Editor.scala - A wrapper for a cardianl editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.js._
import opetopic.tt._
import syntax.complex._

import JQuerySemanticUI._
import Marker.ActiveInstance._
import JsDomFramework._

class Editor {

  type EditorBox[N <: Nat] = ce.CardinalCellBox[N]

  val ce = CardinalEditor[Marker]
  ce.onSelectAsRoot = (boxsig: Sigma[EditorBox]) => {
    rootBox = Some(boxsig)
  }

  var rootBox : Option[Sigma[EditorBox]] = None

  trait BoxAction[A] {
    def objectAction(box : EditorBox[_0]) : EditorM[A]
    def cellAction[P <: Nat](p : P)(box: EditorBox[S[P]]) : EditorM[A]
  }

  @natElim
  def dispatchAction[A, N <: Nat](n: N)(box: EditorBox[N], action: BoxAction[A]) : EditorM[A] = {
    case (Z, box, action) => action.objectAction(box)
    case (S(p), box, action) => action.cellAction(p)(box)
  }

  def withSelection[A](action: BoxAction[A]) : EditorM[A] = 
    rootBox match {
      case None => editorError("Nothing selected")
      case Some(boxsig) => dispatchAction(boxsig.n)(boxsig.value, action)
    }

  object ExtractMarkers extends IndexedTraverse[EditorM, EditorBox, Marker] {
    def apply[N <: Nat](n: N)(box: EditorBox[N]) : EditorM[Marker[N]] =
      attempt(box.optLabel, "Shell is incomplete")
  }

  object ExtractExprs extends IndexedTraverse[EditorM, EditorBox, ConstExpr] {
    def apply[N <: Nat](n: N)(box: EditorBox[N]) : EditorM[Expr] = 
      for { mk <- ExtractMarkers(n)(box) } yield mk.expr
  }

  def frameComplex[P <: Nat](box: EditorBox[S[P]]) : EditorM[ExprComplex[P]] = 
    for {
      fc <- fromShape(box.faceComplex)
      res <- fc.tail.traverse(ExtractExprs)
    } yield res

  def markerComplex[P <: Nat](box: EditorBox[S[P]]) : EditorM[Complex[Marker, P]] = 
    for {
      fc <- fromShape(box.faceComplex)
      res <- fc.tail.traverse(ExtractMarkers)
    } yield res

}
