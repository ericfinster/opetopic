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

import scalaz.std.string._

import opetopic._
import opetopic.js._
import opetopic.tt._
import syntax.complex._
import syntax.nesting._

import JQuerySemanticUI._
import Marker.ActiveInstance._
import JsDomFramework._
import OTTTypeChecker._

class Editor(wksp: DefinitionWorkspace) {

  type EditorBox[N <: Nat] = ce.CardinalCellBox[N]

  def catExpr: Expr = wksp.catExpr

  val ce = CardinalEditor[Marker]
  ce.onSelectAsRoot = (boxsig: Sigma[EditorBox]) => {
    rootBox = Some(boxsig)
  }

  var rootBox : Option[Sigma[EditorBox]] = None

  trait BoxAction[A] {
    def objectAction(box : EditorBox[_0]) : EditorM[A]
    def cellAction[P <: Nat](p : P)(box: EditorBox[S[P]]) : EditorM[A]
  }

  trait LiftAction[A] {
    def apply[P <: Nat](p: P)(box: EditorBox[S[S[P]]]): EditorM[A]
  }

  @natElim
  def dispatchAction[A, N <: Nat](n: N)(box: EditorBox[N], action: BoxAction[A]) : EditorM[A] = {
    case (Z, box, action) => action.objectAction(box)
    case (S(p), box, action) => action.cellAction(p)(box)
  }

  @natElim
  def dispatchLiftAction[A, N <: Nat](n: N)(box: EditorBox[N], action: LiftAction[A]) : EditorM[A] = {
    case (Z, _, _) => editorError("No lift action on an object")
    case (S(Z), _, _) => editorError("No lift action for an arrow")
    case (S(S(p)), box, action) => action(p)(box)
  }

  def withSelection[A](action: BoxAction[A]) : EditorM[A] = 
    for {
      boxsig <- attempt(rootBox, "No box selected")
      a <- dispatchAction(boxsig.n)(boxsig.value, action)
    } yield a

  def liftAtSelection[A](action: LiftAction[A]) : EditorM[A] = 
    for {
      boxsig <- attempt(rootBox, "No box selected")
      a <- dispatchLiftAction(boxsig.n)(boxsig.value, action)
    } yield a

  object ExtractMarkers extends IndexedTraverse[EditorM, EditorBox, Marker] {
    def apply[N <: Nat](n: N)(box: EditorBox[N]) : EditorM[Marker[N]] =
      attempt(box.optLabel, "Shell is incomplete")
  }

  object ExtractExprs extends IndexedTraverse[EditorM, EditorBox, ConstExpr] {
    def apply[N <: Nat](n: N)(box: EditorBox[N]) : EditorM[Expr] = 
      for { mk <- ExtractMarkers(n)(box) } yield mk.expr
  }

  object SuiteExtractExprs extends IndexedTraverse[EditorM, BNst, ENst] {
    def apply[N <: Nat](n: N)(bnst: BNst[N]) : EditorM[ENst[N]] = 
      bnst.traverse[EditorM, Expr]({
        case b => for { 
          mk <- attempt(b.optLabel, "Unexpected missing expression") 
        } yield mk.expr
      })
  }

  @natElim
  def typeExpr[N <: Nat](n: N)(box: EditorBox[N]) : EditorM[Expr] = {
    case (Z, box) => editorSucceed(EOb(catExpr))
    case (S(p), box) => ???
      // for {
      //   frm <- frameComplex(box)
      // } yield ECell(catExpr, frm)
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
  type ENst[N <: Nat] = Nesting[ConstExpr[N], N]
  type VNst[N <: Nat] = Nesting[ConstVal[N], N]
  type PNst[N <: Nat] = Nesting[(EditorBox[N], ConstVal[N]), N]
  type BVPair[N <: Nat] = (BNst[N], VNst[N])

  def pasteToCursor(e: Expr, cat: Val, id: String) : EditorM[Unit] =
    withSelection(new BoxAction[Unit] {

      val rho = wksp.rho
      val gma = wksp.gma

      def objectAction(box: EditorBox[_0]) : EditorM[Unit] = 
        for {
          _ <- forceNone(box.optLabel, "Destination box is not empty")
          _ <- simpleCheck(
            check(rho, gma, e, Ob(cat))
          )
        } yield {

          val mk = ObjectMarker(wksp, id, e)
          box.optLabel = Some(mk)
          box.panel.refresh
          ce.refreshGallery

        }

      def cellAction[P <: Nat](p: P)(box: EditorBox[S[P]]) : EditorM[Unit] = {

        import TypeLemmas._

        // for {
        //   _ <- forceNone(box.optLabel, "Destination box is not empty")
        //   vsig <- simpleCheck(
        //     for {
        //       ty <- checkI(rho, gma, e)
        //       (cv, vsig) <- extCellG(ty)
        //       _ <- eqNf(lRho(rho), cv, cat)
        //     } yield vsig
        //   )
        //   ev <- attempt(matchNatPair(vsig.n, p), "Expression has wrong dimension")
        //   vfrm = rewriteNatIn[ValComplex, vsig.N, P](ev)(vsig.value)
        //   fc <- fromShape(box.faceComplex)
        //   zc = Suite.zip[BNst, VNst, S[P]](fc.tail, vfrm)
        //   pnst <- Suite.traverse[EditorM, BVPair, PNst, S[P]](zc)(Matcher)
        // } yield {

        //   // Update all the faces
        //   Suite.foreach[PNst, S[P]](pnst)(Updater)

        //   // Update the main cell
        //   val mk = CellMarker(p)(wksp, id, e)
        //   box.optLabel = Some(mk)
        //   box.panel.refresh
        //   ce.refreshGallery

        // }

        ???

      }

    })

  object Matcher extends IndexedTraverse[EditorM, BVPair, PNst] {
    def apply[N <: Nat](n: N)(pr: BVPair[N]) : EditorM[PNst[N]] = {

      val rho = wksp.rho
      val gma = wksp.gma
      val nfMap = wksp.nfMap

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
          val nf = rbV(lRho(wksp.rho), v)
          if (wksp.nfMap.isDefinedAt(nf)) {
            val (id, e) = wksp.nfMap(nf)
            b.optLabel = Some(Marker(n)(wksp, id, e))
          } else b.optLabel = Some(Marker(n)(wksp, "unknown", EEmpty))
        }
      })
      pr.baseValue._1.panel.refresh
    }
  }

}
