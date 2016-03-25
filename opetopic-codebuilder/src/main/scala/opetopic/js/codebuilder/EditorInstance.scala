/**
  * EditorInstance.scala - Encapsulate Editor Cababilities
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

import opetopic._
import opetopic.js._
import opetopic.tt._
import opetopic.syntax.all._

import scala.collection.mutable.HashMap

import scalaz.\/
import scalaz.\/-
import scalaz.-\/

import scalaz.std.option._
import scalaz.std.string._

import JsDomFramework._
import Cell.ActiveInstance._
import OpetopicTypeChecker._

class EditorInstance(reg : Registry, env: Environment) {

  type EditorBox[N <: Nat] = editor.CardinalCellBox[N]

  val editor = CardinalEditor[Cell]
  editor.onSelectAsRoot = onSelectAsRoot

  var currentBox: Option[Sigma[EditorBox]] = None

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
    currentBox match {
      case None => editorError("Nothing selected")
      case Some(boxsig) => dispatchAction(boxsig.n)(boxsig.value, action)
    }

  def onSelectAsRoot(boxsig: Sigma[editor.CardinalCellBox]) : Unit = {
    currentBox = Some(boxsig)
  }

  object EditorBoxToExpr extends IndexedMap[EditorBox, ConstExpr] {
    def apply[N <: Nat](n: N)(box: EditorBox[N]) : Expr =
      box.optLabel match {
        case Some(cell) => cell.expr
        case None => EEmpty
      }
  }

  object ExtractCells extends IndexedTraverse[EditorM, EditorBox, Cell] {
    def apply[N <: Nat](n: N)(box: EditorBox[N]) : EditorM[Cell[N]] =
      attempt(box.optLabel, "Unexpected cell extraction failure")
  }

  // You could really clean things up with more of
  // this kind of stuff ...
  class SuccBoxOps[P <: Nat](box: EditorBox[S[P]]) {

    def frameComplex : EditorM[ExprComplex[P]] =
      for {
        fc <- fromShape(box.faceComplex)
      } yield fc.tail.map(EditorBoxToExpr)

    def cellComplex : EditorM[Complex[Cell, P]] =
      for {
        fc <- fromShape(box.faceComplex)
        res <- fc.tail.traverse(ExtractCells)
      } yield res

  }

  def selectedBox : EditorM[Sigma[EditorBox]] = 
    fromOption(currentBox, "Nothing selected")

  def selectionFrame : EditorM[FiniteComplex[Cell]] = {

    @natElim
    def extractFrame[N <: Nat](n: N)(box: EditorBox[N]) : EditorM[FiniteComplex[Cell]] = {
      case (Z, box) => editorError("Object does not have a frame")
      case (S(p: P), box) => for { c <- new SuccBoxOps(box).cellComplex } yield c
    }

    for {
      boxsig <- selectedBox
      frm <- extractFrame(boxsig.n)(boxsig.value)
    } yield frm

  }

  @natElim
  def faceToCell[N <: Nat](n: N)(id: String, expr: Expr, face: Complex[EditorBox, N]) : EditorM[Cell[N]] = {
    case (Z, id, expr, _) => editorSucceed(ObjectCell(id, expr))
    case (S(p: P), id, expr, face) => {
      for {
        cellCmplx <- face.tail.traverse(ExtractCells)
      } yield HigherCell(id, expr, cellCmplx)
    }
  }

  //============================================================================================
  // SVG EXPORTING
  //

  def selectionToSvg: Option[String] =
    for {
      boxsig <- currentBox
      lc <- toOpt(boxsig.value.labelComplex)
    } yield { new SvgExporter(lc).svgString }

  //============================================================================================
  // ASSUMPTIONS
  //

  def assumeObject(box: EditorBox[_0], id: String) : EditorM[Unit] = 
    for {
      _ <- forceNone(box.optLabel, "Cell is occupied")
    } yield {

      val cell = ObjectCell(id, EVar(id))

      env.gma = (id, env.eval(cell.ty)) :: env.gma
      env.rho = UpVar(env.rho, PVar(id), env.genV)
      reg.registerCell(cell)
      reg.registerParameter(cell)

      box.optLabel = Some(cell)
      box.panel.refresh
      editor.refreshGallery

    }

  def assumeCell[P <: Nat](p: P)(box: EditorBox[S[P]], id: String, isLex: Boolean, rexAddrOpt: Option[Address[P]]): EditorM[Unit] = ???
    // for {
    //   _ <- forceNone(box.optLabel, "Cell is occupied")
    //   cellCmplx <- new SuccBoxOps(box).cellComplex
    //   frmCmplx <- new SuccBoxOps(box).frameComplex
    //   varType = ECell(env.catExpr, frmCmplx)

    //   // Make sure the type is valid
    //   _ <- runCheck(

    //     env.checkT(varType)

    //   )(msg => {

    //     editorError("Type checking error: " ++ msg)

    //   })(_ => {

    //     env.extendContext(id, env.eval(varType))
    //     env.extendEnvironment(id, env.genV)

    //     val cell = HigherCell[P](id, EVar(id), cellCmplx)
    //     reg.registerCell(cell)
    //     reg.registerParameter(cell)

    //     cell.isLeftExt =
    //       if (isLex) {
    //         val lexId = id ++ "-is-lex"
    //         val lexType = ELeftExt(EVar(id))
    //         env.extendContext(lexId, env.eval(lexType))
    //         env.extendEnvironment(lexId, env.genV)
    //         val lexProp = IsLeftExtension(lexId, EVar(lexId), cell)
    //         reg.registerProperty(lexProp)
    //         reg.registerParameter(lexProp)
    //         Some(lexProp)
    //       } else None

    //     cell.isRightExt = 
    //       rexAddrOpt match {
    //         case None => None
    //         case Some(rexAddr) => {
    //           val rexId = id ++ "-is-rex"
    //           val rexType = ERightExt(EVar(id), rbAddr(p)(rexAddr))
    //           env.extendContext(rexId, env.eval(rexType))
    //           env.extendEnvironment(rexId, env.genV)
    //           val rexProp = IsRightExtension(rexId, EVar(rexId), cell, rexAddr)
    //           reg.registerProperty(rexProp)
    //           reg.registerParameter(rexProp)
    //           Some(rexProp)
    //         }
    //       }

    //     box.optLabel = Some(cell)
    //     box.panel.refresh
    //     editor.refreshGallery

    //     editorSucceed(())

    //   })
    // } yield ()

  // //============================================================================================
  // // COMPOSITION
  // //

  def composeDiagram(id: String): EditorM[Unit] = editorSucceed(())
  //   for {
  //     boxsig <- attempt(currentBox, "Nothing selected")
  //     fc <- fromShape(boxsig.value.faceComplex)
  //     _ <- doCompose(boxsig.n)(fc, id) 
  //   } yield ()

  // @natElim
  // def doCompose[N <: Nat](n: N)(cmplx: Complex[EditorBox, N], id: String) : EditorM[Unit] = {
  //   case (Z, cmplx, id) => editorError("Dimension too low to compose")
  //   case (S(p: P), fillCmplx @ Complex(Complex(_, Box(compBox, bcn)), Dot(fillBox, _)), id) => {

  //     val idDef = id ++ "-def"

  //     for {
  //       _ <- forceNone(compBox.optLabel, "Composite cell not empty")
  //       _ <- forceNone(fillBox.optLabel, "Filling cell not empty")
  //       compCmplx <- fromShape(fillCmplx.target)
  //       (web, cn) <- (
  //         fillCmplx.map(EditorBoxToExpr).tail match {
  //           case Complex(web, Box(_, cn)) => editorSucceed((web, cn))
  //           case _ => editorError("Malformed tail ...")
  //         }
  //       )

  //       pd = cn.map(_.baseValue)
  //       comp = EComp(env.catExpr, web, pd)
  //       fill = EFill(env.catExpr, web, pd)
  //       fillLeftExt = EFillerLeftExt(env.catExpr, web, pd) // You're not typechecking this?

  //       compCell <- faceToCell(p)(id, comp, compCmplx)
  //       compType = compCell.ty
  //       _ = compBox.optLabel = Some(compCell)

  //       fillCell <- faceToCell(S(p))(idDef, fill, fillCmplx)
  //       fillType = fillCell.ty
  //       _ = fillBox.optLabel = Some(fillCell)

  //       _ <- runCheck(
  //         for {
  //           _ <- env.checkT(compType)
  //           compVal = env.eval(compType)
  //           _ <- env.check(comp, compVal)
  //           _ <- env.checkT(fillType)
  //           fillVal = env.eval(fillType)
  //           _ <- env.check(fill, fillVal)
  //         } yield {

  //           env.extendContext(id, compVal)
  //           env.extendContext(idDef, fillVal)
  //           env.extendEnvironment(id, env.eval(comp))
  //           env.extendEnvironment(idDef, env.eval(fill))

  //         }
  //       )(msg => {

  //         compBox.optLabel = None
  //         fillBox.optLabel = None

  //         editorError("Type checking error: " ++ msg)

  //       })(_ => {

  //         @natElim
  //         def doCompLeftExt[K <: Nat](k: K)(c: Cell[K]) : Unit = {
  //           case (Z, _) => ()
  //           case (S(p), c) => {
  //             c.isLeftExt =
  //               for {
  //                 prTr <- bcn.traverse({
  //                   case b =>
  //                     for {
  //                       cell <- b.baseValue.optLabel
  //                       lextEv <- cell.isLeftExt
  //                     } yield EPair(cell.expr, lextEv.expr)
  //                 })
  //               } yield IsLeftExtension(
  //                 id ++ "-is-lex", 
  //                 EFillerCompLeftExt(env.catExpr, web, prTr), c
  //               )
  //           }
  //         }

  //         doCompLeftExt(compCell.dim)(compCell)

  //         fillCell.isLeftExt = Some(
  //           IsLeftExtension(
  //             idDef ++ "-is-lex",
  //             fillLeftExt,
  //             fillCell
  //           )
  //         )

  //         // Not sure why I need this ...
  //         compBox.optLabel = Some(compCell)
  //         fillBox.optLabel = Some(fillCell)

  //         reg.registerCell(compCell)
  //         reg.registerCell(fillCell)

  //         for { p <- compCell.isLeftExt } { reg.registerProperty(p) }
  //         for { p <- fillCell.isLeftExt } { reg.registerProperty(p) }

  //         compBox.panel.refresh
  //         fillBox.panel.refresh
  //         editor.refreshGallery

  //         editorSucceed(())

  //       })
  //     } yield ()
  //   }
  // }

  // //============================================================================================
  // // PASTING
  // //

  // type BNst[N <: Nat] = Nesting[EditorBox[N], N]
  // type CNst[N <: Nat] = Nesting[Cell[N], N]
  // type PNst[N <: Nat] = Nesting[(EditorBox[N], Cell[N]), N]
  // type BCPair[N <: Nat] = (BNst[N], CNst[N])

  // @natElim
  // def doPaste[N <: Nat](n: N)(cell: Cell[N]): EditorM[Unit] = {
  //   case (Z, cell) => {

  //     import TypeLemmas._

  //     for {
  //       boxsig <- attempt(currentBox, "Nothing Selected")
  //       ev <- attempt(matchNatPair(boxsig.n, Z), "Wrong dimension")
  //       box = rewriteNatIn[EditorBox, boxsig.N, _0](ev)(boxsig.value)
  //       _ <- forceNone(box.optLabel, "Destination box is not empty")
  //       _ = {
  //         box.optLabel = Some(cell)
  //         box.panel.refresh
  //         editor.refreshGallery
  //       }
  //     } yield ()

  //   }
  //   case (S(p: P), cell) => {

  //     import TypeLemmas._

  //     for {
  //       boxsig <- attempt(currentBox, "Nothing Selected")
  //       ev <- attempt(matchNatPair(boxsig.n, S(p)), "Wrong dimension")
  //       box = rewriteNatIn[EditorBox, boxsig.N, S[P]](ev)(boxsig.value)
  //       fc <- fromShape(box.faceComplex)
  //       _ <- forceNone(box.optLabel, "Destination box is not empty")
  //       zc = Suite.zip[BNst, CNst, S[S[P]]](fc, cell.face)
  //       pnst <- Suite.traverse[EditorM, BCPair, PNst, S[S[P]]](zc)(Matcher) 
  //       _ = {
  //         Suite.foreach[PNst, S[S[P]]](pnst)(Updater)
  //         editor.refreshGallery
  //       }
  //     } yield ()

  //   }
  // }

  // object Updater extends IndexedOp[PNst] {
  //   def apply[N <: Nat](n: N)(pr: PNst[N]): Unit = {
  //     pr.foreach({ case (b, c) => b.optLabel = Some(c) })
  //     pr.baseValue._1.panel.refresh
  //   }
  // }

  // object Matcher extends IndexedTraverse[EditorM, BCPair, PNst] {
  //   def apply[N <: Nat](n: N)(pr: BCPair[N]) : EditorM[PNst[N]] = {

  //     val (bnst, cnst) = pr
  //     val fillings: HashMap[EditorBox[N], Expr] = HashMap.empty

  //     fromShape(
  //       Nesting.matchTraverse(bnst, cnst)({
  //         case (b, c) =>
  //           b.optLabel match {
  //             case None => {
  //               if (fillings.isDefinedAt(b)) {
  //                 if (fillings(b) != c.expr) {
  //                   opetopic.fail("Loop conflict")
  //                 } else opetopic.succeed((b, c))
  //               } else {
  //                 fillings(b) = c.expr
  //                 opetopic.succeed((b, c))
  //               }
  //             }
  //             case Some(d) =>
  //               if (d.expr == c.expr) // Or something similar ...
  //                 succeed((b, c))
  //               else
  //                 opetopic.fail("Expression mismatch.")
  //           }
  //       })
  //     )

  //   }
  // }

  // //============================================================================================
  // // LEFT LIFTING
  // //

  def leftLift(id: String) : EditorM[Unit] = editorSucceed(())
  //   for {
  //     boxsig <- attempt(currentBox, "Nothing Selected")
  //     fc <- fromShape(boxsig.value.faceComplex)
  //     _ <- doLeftLift(boxsig.n)(fc, id) 
  //   } yield ()

  // @natElim
  // def doLeftLift[N <: Nat](n: N)(cmplx: Complex[EditorBox, N], id: String): EditorM[Unit] = {
  //   case (Z, _, _) => editorError("Cannot lift here")
  //   case (S(p: P), _, _) => editorError("Cannot lift here")
  //   case (S(S(p: P)), Complex(Complex(tl, Box(liftingBox, cn)), Dot(fillBox, _)), id) => {

  //     val idDef = id ++ "-def"

  //     for {
  //       liftingCell <- attempt(liftingBox.optLabel, "Lifting cell is empty")
  //       (lexBox, emptyBox) <- (
  //         cn.nodes match {
  //           case emptyNst :: lexNst :: Nil => editorSucceed((lexNst.baseValue, emptyNst.baseValue))
  //           case _ => editorError("Must have exactly two source cells")
  //         }
  //       )
  //       lexCell <- attempt(lexBox.optLabel, "Missing left extension cell")
  //       _ <- forceNone(emptyBox.optLabel, "Lift target is not empty")
  //       lexEv <- (
  //         lexCell.isLeftExt match {
  //           case Some(p : IsLeftExtension[P]) => editorSucceed(p)
  //           case _ => editorError("Missing left extension evidence")
  //         }
  //       )
  //       extBox = tl.head.baseValue
  //       extCell <- attempt(extBox.optLabel, "Extension cell is empty")
  //       (ce, frm) <- (
  //         lexCell.ty match {
  //           case ECell(ce, frm) => editorSucceed((ce, frm))
  //           case _ => editorError("Left extension cell has unexpected frame type")
  //         }
  //       )

  //       // Lift setup is valid, build the lift

  //       nchFrm : ExprComplex[P] = tl.map(EditorBoxToExpr)
  //       nch : Tree[Expr, S[P]] = cn.map((n: Nesting[EditorBox[S[P]], S[P]]) => {
  //         n.baseValue.optLabel match {
  //           case None => EEmpty
  //           case Some(c) => c.expr
  //         }
  //       })

  //       rexAddr <- attempt(
  //         nch.mapWithAddress({
  //           case (EEmpty, a) => Some(a)
  //           case (_, a) => None
  //         }).nodes.filter(_.isDefined).head,
  //         "Failed to retrieve extension address"
  //       )

  //       leftBal = EApp(ELeftBal(ce, frm, lexCell.expr, lexEv.expr), extCell.expr)
  //       lift = EApp(ELift(ce, nchFrm, nch, leftBal), liftingCell.expr)
  //       liftFiller = EApp(ELiftFiller(ce, nchFrm, nch, leftBal), liftingCell.expr)
  //       liftFillerLeftExt = EApp(ELiftFillerLeftExt(ce, nchFrm, nch, leftBal), liftingCell.expr)
  //       liftFillerRightExt = EApp(EApp(EApp(
  //         EFillerLeftIsRight(ce, nchFrm, nch, leftBal), liftingCell.expr),
  //         liftFiller), liftFillerLeftExt)

  //       liftFrm <- new SuccBoxOps(emptyBox).cellComplex  
  //       liftCell = HigherCell(id, lift, liftFrm)
  //       _ = { emptyBox.optLabel = Some(liftCell) }

  //       fillFrm <- new SuccBoxOps(fillBox).cellComplex
  //       fillCell = HigherCell(idDef, liftFiller, fillFrm)
  //       _ = {
  //         fillCell.isLeftExt = Some(
  //           IsLeftExtension(idDef ++ "-is-lex", liftFillerLeftExt, fillCell))
  //         fillCell.isRightExt = Some(
  //           IsRightExtension(idDef ++ "-is-rex", liftFillerRightExt, fillCell, rexAddr))
  //         fillBox.optLabel = Some(fillCell)
  //       }

  //       // Setup the types ...
  //       leftBalTy = EBal(ce, nchFrm, nch)
  //       liftTy = liftCell.ty
  //       liftFillerTy = fillCell.ty
  //       liftFillerLeftExtTy = ELeftExt(liftFiller)

  //       _ <- runCheck(
  //         for {
  //           _ <- env.checkT(leftBalTy)
  //           leftBalVal = env.eval(leftBalTy)
  //           _ <- env.check(leftBal, leftBalVal)
  //           _ <- env.checkT(liftTy)
  //           liftTyVal = env.eval(liftTy)
  //           _ <- env.check(lift, liftTyVal)
  //           _ <- env.checkT(liftFillerTy)
  //           liftFillerTyVal = env.eval(liftFillerTy)
  //           _ <- env.check(liftFiller, liftFillerTyVal)
  //         } yield {
  //           env.extendContext(id, liftTyVal)
  //           env.extendContext(idDef, liftFillerTyVal)
  //           env.extendEnvironment(id, env.eval(lift))
  //           env.extendEnvironment(idDef, env.eval(liftFiller))
  //         }
  //       )(msg => { // On Error

  //         fillBox.optLabel = None
  //         emptyBox.optLabel = None

  //         editorError("Lift failed: " ++ msg)

  //       })(_ => { // On succeed

  //         reg.registerCell(liftCell)
  //         reg.registerCell(fillCell)

  //         for { p <- fillCell.isLeftExt } { reg.registerProperty(p) }
  //         for { p <- fillCell.isRightExt } { reg.registerProperty(p) }

  //         emptyBox.panel.refresh
  //         fillBox.panel.refresh
  //         editor.refreshGallery

  //         editorSucceed(())

  //       })
  //     } yield ()
  //   }
  //   case (S(S(p: P)), _, _) => editorError("Malformed lift")
  // }

  // //============================================================================================
  // // RIGHT LIFTING
  // //

  def rightLift(id: String) : EditorM[Unit] = editorSucceed(())
  //   for {
  //     boxsig <- attempt(currentBox, "Nothing Selected")
  //     fc <- fromShape(boxsig.value.faceComplex)
  //     _ <- doRightLift(boxsig.n)(fc, id) 
  //   } yield ()

  // @natElim
  // def doRightLift[N <: Nat](n: N)(cmplx: Complex[EditorBox, N], id: String): EditorM[Unit] = {
  //   case (Z, _, _) => editorError("Cannot lift here")
  //   case (S(p: P), _, _) => editorError("Cannot lift here")
  //   case (S(S(p: P)), Complex(Complex(tl, Box(liftingBox, cn)), Dot(fillBox, _)), id) => {

  //     val idDef = id ++ "-def"

  //     for {
  //       liftingCell <- attempt(liftingBox.optLabel, "Empty lifting cell")
  //       (rexBox, emptyBox) <- (
  //         cn.nodes match {
  //           case rexNst :: emptyNst :: Nil => editorSucceed((rexNst.baseValue, emptyNst.baseValue))
  //           case _ => editorError("Must have exactly two source cells")
  //         }
  //       )
  //       rexCell <- attempt(rexBox.optLabel, "Missing right extension cell")
  //       _ <- forceNone(emptyBox.optLabel, "Target for lift is occupied")
  //       rexEv <- (
  //         rexCell.isRightExt match {
  //           case Some(p : IsRightExtension[P]) => editorSucceed(p)
  //           case _ => editorError("Missing right extension evidence")
  //         }
  //       )
  //       tgtZipper <- fromShape(tl.head.seekTo(rexEv.addr :: Nil), 
  //         msg => "Failed to find extension cell because of shape error: " ++ msg
  //       )
  //       (baseBox, extBox) <- (
  //         tgtZipper._1 match {
  //           case Box(baseBox, cn) => 
  //             cn.nodes match {
  //               case extNst :: Nil => editorSucceed((baseBox, extNst.baseValue))
  //               case _ => editorError("Extension is not globular")
  //             }
  //           case _ => editorError("Extension cell was external")
  //         }
  //       )
  //       baseCell <- attempt(baseBox.optLabel, "Base extension cell is empty")
  //       extCell <- attempt(extBox.optLabel, "Extending source cell is empty")
  //       (ce, frm) <- (
  //         rexCell.ty match {
  //           case ECell(ce, frm) => editorSucceed((ce, frm))
  //           case _ => editorError("Right extension has unexpected frame type")
  //         }
  //       )

  //       // Everything should be in scope.  Time to construct the lift.

  //       nchFrm : ExprComplex[P] = tl.map(EditorBoxToExpr)
  //       nch : Tree[Expr, S[P]] = cn.map((n: Nesting[EditorBox[S[P]], S[P]]) => {
  //         n.baseValue.optLabel match {
  //           case None => EEmpty
  //           case Some(c) => c.expr
  //         }
  //       })

  //       rexAddr <- fromOption(
  //         nch.mapWithAddress({
  //           case (EEmpty, a) => Some(a)
  //           case (_, a) => None
  //         }).nodes.filter(_.isDefined).head,
  //         "Failed to isolate new right extension address"
  //       )

  //       rightBal = EApp(ERightBal(ce, frm, rexCell.expr, rbAddr(p)(rexEv.addr), rexEv.expr), extCell.expr)
  //       lift = EApp(ELift(ce, nchFrm, nch, rightBal), liftingCell.expr)
  //       liftFiller = EApp(ELiftFiller(ce, nchFrm, nch, rightBal), liftingCell.expr)
  //       liftFillerLeftExt = EApp(ELiftFillerLeftExt(ce, nchFrm, nch, rightBal), liftingCell.expr)
  //       liftFillerRightExt = EApp(EApp(EApp(
  //         EFillerLeftIsRight(ce, nchFrm, nch, rightBal), liftingCell.expr),
  //         liftFiller), 
  //         liftFillerLeftExt
  //       )

  //       liftFrm <- new SuccBoxOps(emptyBox).cellComplex
  //       liftCell = HigherCell(id, lift, liftFrm)
  //       _ = { emptyBox.optLabel = Some(liftCell) }

  //       fillFrm <- new SuccBoxOps(fillBox).cellComplex
  //       fillCell = HigherCell(idDef, liftFiller, fillFrm)
  //       _ = { 
  //         fillCell.isLeftExt = Some(
  //           IsLeftExtension(idDef ++ "-is-lex", liftFillerLeftExt, fillCell))
  //         fillCell.isRightExt = Some(
  //           IsRightExtension(idDef ++ "-is-rex", liftFillerRightExt, fillCell, rexAddr))
  //         fillBox.optLabel = Some(fillCell)
  //       }

  //       // Setup the types to check against
  //       rightBalTy = EBal(ce, nchFrm, nch)
  //       liftTy = liftCell.ty
  //       liftFillerTy = fillCell.ty
  //       liftFillerLeftExtTy = ELeftExt(liftFiller)
  //       liftFillerRightExtTy = ERightExt(liftFiller, rbAddr(S(p))(rexAddr))

  //       // Typecheck
  //       _ <- runCheck(
  //         for {
  //           _ <- env.checkT(rightBalTy)
  //           rightBalVal = env.eval(rightBalTy)
  //           _ <- env.check(rightBal, rightBalVal)
  //           _ <- env.checkT(liftTy)
  //           liftTyVal = env.eval(liftTy)
  //           _ <- env.check(lift, liftTyVal)
  //           _ <- env.checkT(liftFillerTy)
  //           liftFillerTyVal = env.eval(liftFillerTy)
  //           _ <- env.check(liftFiller, liftFillerTyVal)
  //           // And you should do the witnessess too ....
  //         } yield {
  //           env.extendContext(id, liftTyVal)
  //           env.extendContext(idDef, liftFillerTyVal)
  //           env.extendEnvironment(id, env.eval(lift))
  //           env.extendEnvironment(idDef, env.eval(liftFiller))
  //         }
  //       )(msg => { // On Error

  //         fillBox.optLabel = None
  //         emptyBox.optLabel = None

  //         editorError("Typechecking error: " ++ msg)

  //       })(_ => { // On Success

  //         reg.registerCell(liftCell)
  //         reg.registerCell(fillCell)

  //         for { p <- fillCell.isLeftExt } { reg.registerProperty(p) }
  //         for { p <- fillCell.isRightExt } { reg.registerProperty(p) }

  //         emptyBox.panel.refresh
  //         fillBox.panel.refresh
  //         editor.refreshGallery

  //         editorSucceed(())

  //       })
  //     } yield ()

  //   }
  //   case (S(S(p: P)), _, _) => editorError("Malformed lift")
  // }

  // //============================================================================================
  // // RIGHT ASSERTION
  // //

  def assertRightExtension: EditorM[Unit] = editorSucceed(())
  //   for {
  //     boxsig <- attempt(currentBox, "Nothing Selected")
  //     fc <- fromShape(boxsig.value.faceComplex)
  //     _ <- doRightAssertion(boxsig.n)(fc) 
  //   } yield ()
    
  // // BUG!!! - We do not quite check that the shape is right.  (Presumably
  // // this would be caught by the typechecker.)  The point is that the "liftBox"
  // // must be unary, since left extensions are only defined for unary outgoing
  // // cells.

  // @natElim
  // def doRightAssertion[N <: Nat](n: N)(cmplx: Complex[EditorBox, N]): EditorM[Unit] = {
  //   case (Z, _) => editorError("Dimension is too low")
  //   case (S(p: P), _) => editorError("Dimension is too low")
  //   case (S(S(p: P)), Complex(Complex(tl, Box(liftedBox, cn)), Dot(filledBox, _))) => {

  //     for {
  //       liftedCell <- attempt(liftedBox.optLabel, "Lifted cell is empty")
  //       filledCell <- attempt(filledBox.optLabel, "Filled cell is empty")
  //       filledLexEv <- (
  //         filledCell.isLeftExt match {
  //           case Some(flev : IsLeftExtension[S[P]]) => editorSucceed(flev)
  //           case _ => editorError("No left extension evidence")
  //         }
  //       )
  //       (lexBox, liftBox) <- (
  //         cn.nodes match {
  //           case liftNst :: lexNst :: Nil => editorSucceed((lexNst.baseValue, liftNst.baseValue))
  //           case _ => editorError("Must have exactly two source cells")
  //         }
  //       )
  //       lexCell <- attempt(lexBox.optLabel, "Left extension cell is missing")
  //       liftCell <- attempt(liftBox.optLabel, "Lift cell is missing")
  //       lexEv <- (
  //         lexCell.isLeftExt match {
  //           case Some(ev : IsLeftExtension[P]) => editorSucceed(ev)
  //           case _ => editorError("Left extension cell is missing evidence")
  //         }
  //       )
  //       extBox = tl.head.baseValue
  //       extCell <- attempt(extBox.optLabel, "Extension cell is empty")
  //       (ce, frm) <- (
  //         lexCell.ty match {
  //           case ECell(ce, frm) => editorSucceed((ce, frm))
  //           case _ => editorError("Left extension cell has unexpected frame type")
  //         }
  //       )

  //       nchFrm : ExprComplex[P] = tl.map(EditorBoxToExpr)
  //       nch : Tree[Expr, S[P]] = cn.map((n: Nesting[EditorBox[S[P]], S[P]]) => {
  //         val box = n.baseValue  // Is there a better way to do this?
  //         if (box == liftBox) {
  //           EEmpty
  //         } else box.optLabel.get.expr
  //       })

  //       rexAddr <- fromOption(
  //         nch.mapWithAddress({
  //           case (EEmpty, a) => Some(a)
  //           case (_, a) => None
  //         }).nodes.filter(_.isDefined).head,
  //         "Failed to isolate new right extension address"
  //       )

  //       leftBal = EApp(ELeftBal(ce, frm, lexCell.expr, lexEv.expr), extCell.expr)
  //       rexEv = EApp(EApp(EApp(
  //         EFillerLeftIsRight(ce, nchFrm, nch, leftBal), liftedCell.expr),
  //         filledCell.expr), filledLexEv.expr)

  //       // Right, well, he needs to be typechecked ...

  //     } yield {

  //       val rexProp = 
  //         IsRightExtension(filledCell.id ++ "-is-rex", rexEv, filledCell, rexAddr)

  //       filledCell.isRightExt = Some(rexProp)
  //       reg.registerProperty(rexProp)

  //       filledBox.panel.refresh
  //       editor.refreshGallery

  //     }

  //   }
  //   case (S(S(p: P)), _) => editorError("Malformed complex")
  // }



}
