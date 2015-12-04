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

import JsDomFramework._
import Cell.ActiveInstance._
import OpetopicTypeChecker._

class EditorInstance(env: EditorEnvironment) {

  type EditorBox[N <: Nat] = editor.CardinalCellBox[N]

  val editor = CardinalEditor[Cell]
  editor.onSelectAsRoot = onSelectAsRoot

  var currentBox: Option[Sigma[EditorBox]] = None

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
  // ASSUME A VARIABLE
  //

  def assumeVariable(id: String, isLex: Boolean): EditorM[Unit] =
    for {
      boxsig <- attempt(currentBox, "Nothing Selected")
      fc <- fromShape(boxsig.value.faceComplex)
      _ <- doAssume(boxsig.n)(fc, id, isLex)
    } yield ()

  @natElim
  def doAssume[N <: Nat](n: N)(cmplx: Complex[EditorBox, N], id: String, isLex: Boolean) : EditorM[Unit] = {
    case (Z, Complex(_, Obj(b)), id, isLex) =>
      b.optLabel match {
        case None => {

          val cell = ObjectCell(id, EVar(id))

          env.gma = (id, env.eval(cell.ty)) :: env.gma
          env.rho = UpVar(env.rho, PVar(id), env.genV)
          env.registerCell(cell)

          b.optLabel = Some(cell)
          b.panel.refresh
          editor.refreshGallery

          editorSucceed(())

        }
        case Some(_) => editorError("Cell is occupied")
      }
    case (Z, _, _, _) => editorError("Malformed complex")
    case (S(p: P), Complex(tl, Dot(b, _)), id, isLex) =>
      b.optLabel match {
        case None => {

          tl.traverse(ExtractCells) match {
            case \/-(cellCmplx) => {

              val frmCmplx : ExprComplex[P] = tl.map(EditorBoxToExpr)
              val varType : Expr = ECell(env.catExpr, frmCmplx)

              checkT(env.rho, env.gma, varType) match {
                case -\/(msg) => editorError("Error: " ++ msg)
                case \/-(()) => {

                  env.gma = (id, env.eval(varType)) :: env.gma
                  env.rho = UpVar(env.rho, PVar(id), env.genV)

                  val isLexOpt : Option[Expr] =
                    if (isLex) {
                      val lexId = id ++ "-is-lex"
                      val lexType = ELeftExt(EVar(id))
                      env.gma = (lexId, env.eval(lexType)) :: env.gma
                      env.rho = UpVar(env.rho, PVar(lexId), env.genV)
                      Some(EVar(id ++ "-is-lex"))
                    } else None

                  val cell = HigherCell[P](id, EVar(id), cellCmplx)
                  cell.isLeftExt = isLexOpt

                  env.registerCell(cell)

                  b.optLabel = Some(cell)
                  b.panel.refresh
                  editor.refreshGallery

                  editorSucceed(())

                }
              }
            }
            case -\/(_) => editorError("There are non-full cells")
          }

        }
        case Some(_) => editorError("Cell is occupied")
      }
    case (S(p: P), _, _, _) => editorError("Malformed complex")
  }

  //============================================================================================
  // COMPOSE A DIAGRAM
  //

  def composeDiagram(id: String): EditorM[Unit] =
    for {
      boxsig <- attempt(currentBox, "Nothing selected")
      fc <- fromShape(boxsig.value.faceComplex)
      _ <- doCompose(boxsig.n)(fc, id) 
    } yield ()

  // Rewrite this.  It's terrible.
  @natElim
  def doCompose[N <: Nat](n: N)(cmplx: Complex[EditorBox, N], id: String) : EditorM[Unit] = {
    case (Z, cmplx, id) => editorError("Dimension too low to compose")
    case (S(p: P), fillCmplx @ Complex(Complex(_, Box(compBox, bcn)), Dot(fillBox, _)), id) => {

      val idDef = id ++ "-def"

      for {
        _ <- forceNone(compBox.optLabel, "Composite cell not empty")
        _ <- forceNone(fillBox.optLabel, "Filling cell not empty")
        compCmplx <- fromShape(fillCmplx.target)
        pr <- (
          fillCmplx.map(EditorBoxToExpr).tail match {
            case Complex(web, Box(_, cn)) => editorSucceed((web, cn))
            case _ => editorError("Malformed tail ...")
          }
        )

        (web, cn) = pr
        pd = cn.map(_.baseValue)
        comp = EComp(env.catExpr, web, pd)
        fill = EFill(env.catExpr, web, pd)
        fillLeftExt = EFillerLeftExt(env.catExpr, web, pd) // You're not typechecking this?

        compCell <- faceToCell(p)(id, comp, compCmplx)
        compType = compCell.ty
        _ = compBox.optLabel = Some(compCell)

        fillCell <- faceToCell(S(p))(idDef, fill, fillCmplx)
        fillType = fillCell.ty
        _ = fillBox.optLabel = Some(fillCell)

        _ <- runCheck(
          for {
            _ <- env.checkT(compType)
            compVal = env.eval(compType)
            _ <- env.check(comp, compVal)
            _ <- env.checkT(fillType)
            fillVal = env.eval(fillType)
            _ <- env.check(fill, fillVal)
          } yield {

            env.extendContext(id, compVal)
            env.extendContext(idDef, fillVal)
            env.extendEnvironment(id, env.eval(comp))
            env.extendEnvironment(idDef, env.eval(fill))

          }
        )(msg => {

          compBox.optLabel = None
          fillBox.optLabel = None

          editorError("Type checking error: " ++ msg)

        })(_ => {

          // Handle composition of left extensions
          val compLeftExtEv : Option[Expr] =
            for {
              prTr <- bcn.traverse({
                case b =>
                  for {
                    cell <- b.baseValue.optLabel
                    lextEv <- cell.isLeftExt
                  } yield EPair(cell.expr, lextEv)
              })
            } yield EFillerCompLeftExt(env.catExpr, web, prTr)

          compCell.isLeftExt = compLeftExtEv
          fillCell.isLeftExt = Some(fillLeftExt)

          // Not sure why I need this ...
          compBox.optLabel = Some(compCell)
          fillBox.optLabel = Some(fillCell)

          env.registerCell(compCell)
          env.registerCell(fillCell)

          compBox.panel.refresh
          fillBox.panel.refresh
          editor.refreshGallery

          editorSucceed(())

        })
      } yield ()
    }
  }

  //============================================================================================
  // PASTING
  //


  type BNst[N <: Nat] = Nesting[EditorBox[N], N]
  type CNst[N <: Nat] = Nesting[Cell[N], N]
  type PNst[N <: Nat] = Nesting[(EditorBox[N], Cell[N]), N]
  type BCPair[N <: Nat] = (BNst[N], CNst[N])

  object Updater extends IndexedOp[PNst] {
    def apply[N <: Nat](n: N)(pr: PNst[N]): Unit = {
      pr.foreach({ case (b, c) => b.optLabel = Some(c) })
      pr.baseValue._1.panel.refresh
    }
  }

  object Matcher extends IndexedTraverse[EditorM, BCPair, PNst] {
    def apply[N <: Nat](n: N)(pr: BCPair[N]) : EditorM[PNst[N]] = {

      val (bnst, cnst) = pr
      val fillings: HashMap[EditorBox[N], Expr] = HashMap.empty

      fromShape(
        Nesting.matchTraverse(bnst, cnst)({
          case (b, c) =>
            b.optLabel match {
              case None => {
                if (fillings.isDefinedAt(b)) {
                  if (fillings(b) != c.expr) {
                    opetopic.fail("Loop conflict")
                  } else opetopic.succeed((b, c))
                } else {
                  fillings(b) = c.expr
                  opetopic.succeed((b, c))
                }
              }
              case Some(d) =>
                if (d.expr == c.expr) // Or something similar ...
                  succeed((b, c))
                else
                  opetopic.fail("Expression mismatch.")
            }
        })
      )

    }
  }

  @natElim
  def doPaste[N <: Nat](n: N)(cell: Cell[N]): EditorM[Unit] = {
    case (Z, cell) => {

      import TypeLemmas._

      for {
        boxsig <- attempt(currentBox, "Nothing Selected")
        ev <- attempt(matchNatPair(boxsig.n, Z), "Wrong dimension")
        box = rewriteNatIn[EditorBox, boxsig.N, _0](ev)(boxsig.value)
        _ <- forceNone(box.optLabel, "Destination box is not empty")
        _ = {
          box.optLabel = Some(cell)
          box.panel.refresh
          editor.refreshGallery
        }
      } yield ()

    }
    case (S(p: P), cell) => {

      import TypeLemmas._

      for {
        boxsig <- attempt(currentBox, "Nothing Selected")
        ev <- attempt(matchNatPair(boxsig.n, S(p)), "Wrong dimension")
        box = rewriteNatIn[EditorBox, boxsig.N, S[P]](ev)(boxsig.value)
        fc <- fromShape(box.faceComplex)
        _ <- forceNone(box.optLabel, "Destination box is not empty")
        zc = Suite.zip[BNst, CNst, S[S[P]]](fc, cell.face)
        pnst <- Suite.traverse[EditorM, BCPair, PNst, S[S[P]]](zc)(Matcher) 
        _ = {
          Suite.foreach[PNst, S[S[P]]](pnst)(Updater)
          editor.refreshGallery
        }
      } yield ()

    }
  }

  //============================================================================================
  // LIFTING
  //

  def liftCell(id: String) : EditorM[Unit] =
    for {
      boxsig <- attempt(currentBox, "Nothing Selected")
      fc <- fromShape(boxsig.value.faceComplex)
      _ <- doLift(boxsig.n)(fc, id) 
    } yield ()

  @natElim
  def doLift[N <: Nat](n: N)(cmplx: Complex[EditorBox, N], id: String): EditorM[Unit] = {
    case (Z, _, _) => editorError("Cannot lift here")
    case (S(p: P), _, _) => editorError("Cannot lift here")
    case (S(S(p: P)), Complex(Complex(tl, Box(liftBox, cn)), Dot(fillBox, _)), id) => {

      val tgtBox = tl.head.baseValue

      (tgtBox.optLabel, liftBox.optLabel, cn.nodes) match {
        case (Some(tgtCell), Some(liftCell), rootNst :: lexNst :: Nil) => {
          (rootNst.baseValue.optLabel, lexNst.baseValue.optLabel) match {
            case (None, Some(lexCell)) => {
              lexCell.isLeftExt match {
                case Some(lexEv) => {

                  lexCell.ty match {
                    case ECell(ce, frm) => {

                      val idDef = id ++ "-def"
                      val rootBox = rootNst.baseValue

                      val nchFrm : ExprComplex[P] = tl.map(EditorBoxToExpr)
                      val nch : Tree[Expr, S[P]] = cn.map((n: Nesting[EditorBox[S[P]], S[P]]) => {
                        n.baseValue.optLabel match {
                          case None => EEmpty
                          case Some(c) => c.expr
                        }
                      })

                      val leftBal = EApp(ELeftBal(ce, frm, lexCell.expr, lexEv), tgtCell.expr)
                      val lift = EApp(ELift(ce, nchFrm, nch, leftBal), liftCell.expr)
                      val liftFiller = EApp(ELiftFiller(ce, nchFrm, nch, leftBal), liftCell.expr)
                      val liftFillerLeftExt = EApp(ELiftFillerLeftExt(ce, nchFrm, nch, leftBal), liftCell.expr)

                      // An observation:  Here you asign the actual expression to the cell.
                      // But shouldn't you really just use the variable?  You are going to add
                      // it to the context, and I suspect the saves a lot in terms of typechecking....

                      for {

                        frm <- new SuccBoxOps(rootBox).cellComplex  // Implicts aren't working for this ...
                        liftCell = HigherCell(id, lift, frm)
                        _ = rootBox.optLabel = Some(liftCell)
                        fillFrm <- new SuccBoxOps(fillBox).cellComplex
                        fillCell = HigherCell(idDef, liftFiller, fillFrm)
                        _ = fillCell.isLeftExt = Some(liftFillerLeftExt)
                        _ = fillBox.optLabel = Some(fillCell)

                        // Setup the types ...
                        leftBalTy = EBal(ce, nchFrm, nch)
                        liftTy = liftCell.ty
                        liftFillerTy = fillCell.ty
                        liftFillerLeftExtTy = ELeftExt(liftFiller)

                        _ <- runCheck(
                          for {
                            _ <- env.checkT(leftBalTy)
                            leftBalVal = env.eval(leftBalTy)
                            _ <- env.check(leftBal, leftBalVal)
                            _ <- env.checkT(liftTy)
                            liftTyVal = env.eval(liftTy)
                            _ <- env.check(lift, liftTyVal)
                            _ <- env.checkT(liftFillerTy)
                            liftFillerTyVal = env.eval(liftFillerTy)
                            _ <- env.check(liftFiller, liftFillerTyVal)
                          } yield {
                            env.extendContext(id, liftTyVal)
                            env.extendContext(idDef, liftFillerTyVal)
                            env.extendEnvironment(id, env.eval(lift))
                            env.extendEnvironment(idDef, env.eval(liftFiller))
                          }
                        )(msg => { // On Error

                          fillBox.optLabel = None
                          rootBox.optLabel = None

                          editorError("Lift failed: " ++ msg)

                        })(_ => { // On succeed

                          env.registerCell(liftCell)
                          env.registerCell(fillCell)

                          rootBox.panel.refresh
                          fillBox.panel.refresh
                          editor.refreshGallery

                          editorSucceed(())

                        })
                      } yield ()
                    }
                    case _ => editorError("Unexpected: lex cell has a bizzare type")
                  }

                }
                case None => editorError("Cell is not a left extension")
              }
            }
            case _ => editorError("Not a liftable position")
          }
        }
        case _ => editorError("Not a liftable position")
      }
    }
    case (S(S(p: P)), _, _) => editorError("Malformed lift")
  }


}
