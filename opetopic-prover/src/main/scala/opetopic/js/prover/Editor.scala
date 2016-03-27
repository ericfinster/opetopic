/**
  * Editor.scala - A wrapper for a cardianl editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scala.collection.mutable.HashMap

import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.js._
import opetopic.tt._
import syntax.complex._
import syntax.nesting._

import JQuerySemanticUI._
import Marker.ActiveInstance._
import JsDomFramework._
import OpetopicTypeChecker._

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

  //============================================================================================
  // PASTING
  //

  type BNst[N <: Nat] = Nesting[EditorBox[N], N]
  type VNst[N <: Nat] = Nesting[ConstVal[N], N]
  type PNst[N <: Nat] = Nesting[(EditorBox[N], ConstVal[N]), N]
  type BVPair[N <: Nat] = (BNst[N], VNst[N])

  def pasteToCursor(e: Expr, cat: Val, id: String) : EditorM[Unit] =
    withSelection(new BoxAction[Unit] {

      val rho = Prover.rho
      val gma = Prover.gma

      def objectAction(box: EditorBox[_0]) : EditorM[Unit] = 
        for {
          _ <- forceNone(box.optLabel, "Destination box is not empty")
          _ <- simpleCheck(
            check(rho, gma, e, Ob(cat))
          )
        } yield {

          val mk = ObjectMarker(id, e)
          box.optLabel = Some(mk)
          box.panel.refresh
          ce.refreshGallery

        }

      def cellAction[P <: Nat](p: P)(box: EditorBox[S[P]]) : EditorM[Unit] = {

        import TypeLemmas._

        for {
          _ <- forceNone(box.optLabel, "Destination box is not empty")
          vsig <- simpleCheck(
            for {
              ty <- checkI(rho, gma, e)
              (cv, vsig) <- extCellG(ty)
              _ <- eqNf(lRho(rho), cv, cat)
            } yield vsig
          )
          ev <- attempt(matchNatPair(vsig.n, p), "Expression has wrong dimension")
          vfrm = rewriteNatIn[ValComplex, vsig.N, P](ev)(vsig.value)
          fc <- fromShape(box.faceComplex)
          zc = Suite.zip[BNst, VNst, S[P]](fc.tail, vfrm)
          pnst <- Suite.traverse[EditorM, BVPair, PNst, S[P]](zc)(Matcher)
        } yield {

          // Update all the faces
          Suite.foreach[PNst, S[P]](pnst)(Updater)

          // Update the main cell
          val mk = CellMarker(p)(id, e)
          box.optLabel = Some(mk)
          box.panel.refresh
          ce.refreshGallery

        }
      }

    })

  object Matcher extends IndexedTraverse[EditorM, BVPair, PNst] {
    def apply[N <: Nat](n: N)(pr: BVPair[N]) : EditorM[PNst[N]] = {

      val rho = Prover.rho
      val gma = Prover.gma
      val nfMap = Prover.nfMap

      val l = lRho(rho)

      val (bnst, vnst) = pr
      val fillings: HashMap[EditorBox[N], Val] = HashMap.empty

      fromShape(
        Nesting.matchTraverse(bnst, vnst)({
          case (box, v) => 
            box.optLabel match {
              case None => 
                if (fillings.isDefinedAt(box)) {
                  toShape(
                    for {
                      _ <- eqNf(l, fillings(box), v)
                    } yield (box, v)
                  )
                } else {
                  fillings(box) = v
                  opetopic.succeed((box, v))
                }
              case Some(mk) => 
                toShape(
                  for {
                    _ <- eqNf(l, eval(mk.expr, rho), v)
                  } yield (box, v)
                )
            }
        })
      )
    }
  }

  object Updater extends IndexedOp[PNst] {
    def apply[N <: Nat](n: N)(pr: PNst[N]): Unit = {
      pr.foreach({ case (b, v) => 
        if (b.optLabel == None) {
          val nf = rbV(lRho(Prover.rho), v)
          if (Prover.nfMap.isDefinedAt(nf)) {
            val (id, e) = Prover.nfMap(nf)
            b.optLabel = Some(Marker(n)(id, e))
          } else b.optLabel = Some(Marker(n)("unknown", EEmpty))
        }
      })
      pr.baseValue._1.panel.refresh
    }
  }

}
