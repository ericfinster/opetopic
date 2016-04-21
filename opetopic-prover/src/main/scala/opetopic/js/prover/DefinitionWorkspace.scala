/**
  * DefinitionWorkspace.scala - A class for building OpetopicTT definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import org.scalajs.jquery._
import scalatags.JsDom.all._
import opetopic.js.JQuerySemanticUI._
import scala.scalajs.js.Dynamic.{literal => lit}

import scalaz.std.string._

import opetopic._
import opetopic.tt._
import OTTTypeChecker._
import syntax.all._
import opetopic.pprint.Tokenizer._
import PrettyPrinter._
import Prover.runAction

class DefinitionWorkspace(val module: Module) extends DefinitionWorkspaceUI { thisWksp =>

  //============================================================================================
  // CONTEXT MANAGEMENT
  //

  // Map a normal form back to a name and an expression
  val nfMap: HashMap[Expr, (String, Expr)] = HashMap()

  val catExpr: Expr = EVar("X")

  val context: ListBuffer[(String, Expr)] = ListBuffer()
  val environment: ListBuffer[(String, Expr, Expr)] = ListBuffer()
  val cells: ListBuffer[(String, Expr)] = ListBuffer()
  val properties: ListBuffer[Property] = ListBuffer()

  var gma: Gamma = module.gma
  var rho: Rho = module.rho

  def abstractOverContext(gma: List[(String, Expr)], expr: Expr, exprTy: Expr) : (Expr, Expr) = 
    gma match {
      case Nil => (expr, exprTy)
      case (id, ty) :: gs => {
        val (e, t) = abstractOverContext(gs, expr, exprTy)
        (ELam(PVar(id), e), EPi(PVar(id), ty, t))
      }
    }

  def extendContext[N <: Nat](id: String, ty: Expr) : Unit = {

    // Take care of the semantic part
    val tVal = eval(ty, rho)
    val l = lRho(rho)
    val gen = Nt(Gen(l, "TC#"))

    gma = (id, tVal) :: gma
    rho = UpVar(rho, PVar(id), gen)

    context += ((id, ty))
    nfMap(EVar("TC#" ++ l.toString)) = (id, EVar(id))

    val title = div(cls := "title")(
      i(cls := "dropdown icon"), id + " : " + ty.pprint
    ).render

    val content = div(cls := "content")(
      p("No information")
    ).render

    jQuery(contextList).append(title, content)

  }

  def extendEnvironment(id: String, expr: Expr, ty: Expr) : Unit = {

    environment += ((id, expr, ty))

    // Update the normal form map
    val nf = rbV(lRho(rho), eval(expr, rho))
    nfMap(nf) = (id, expr)

    val title = div(cls := "title")(
      i(cls := "dropdown icon"), id
    ).render

    val content = div(cls := "content")(
      p(expr.pprint ++ " : " + ty.pprint),
      button(cls := "ui icon button", onclick := { () => runAction(onExport(id, expr, ty)) })(
        i(cls := "check circle icon")
      )
    ).render

    jQuery(environmentList).append(title, content)

  }

  def registerCell(id: String, expr: Expr) : Unit = {

    cells += ((id, expr))

    val title = div(cls := "title")(
      i(cls := "dropdown icon"), id
    ).render

    val content = div(cls := "content")(
      button(
        cls := "ui icon button",
        onclick := { () => runAction(onPaste(expr, id)) }
      )(
        i(cls := "paste icon")
      )
    ).render

    jQuery(cellList).append(title, content)

  }

  def registerProperty(prop: Property) : Unit = {

    properties += prop

    val title = div(cls := "title")(
      i(cls := "dropdown icon"), prop.propertyId
    ).render

    val content = div(cls := "content")(
      prop match {
        case l : LeftExtensionProperty => List(p("Target cell: " ++ prop.cellId))
        case r : RightExtensionProperty => List(
          p("Target cell: " ++ prop.cellId),
          p("Address: " ++ r.addr.toString)
        )
      }
        
    ).render

    jQuery(propertyList).append(title, content)

  }

  def findLeftExtensionWitness(cid: String) : Option[Expr] = 
    properties.filter(_.cellId == cid).filter(_.isLeft).map(_.propertyExpr).headOption

  def findRightExtensionWitness(cid: String, addr: Addr) : Option[Expr] = 
    properties.filter(_.cellId == cid).filter(_.isRightAt(addr)).map(_.propertyExpr).headOption

  def hasUniversalProperty(cid: String) : Boolean = 
    properties.filter(_.cellId == cid).length > 0 

  //============================================================================================
  // EDITOR EXTENSIONS
  //

  def runInstanceAction[T](act: InstanceAction[EditorM[T]]) : EditorM[T] = 
    for {
      a <- attempt(actionWithSelection(act), "No selection")
      r <- a
    } yield r


  def frameExpression[P <: Nat](i: EditorInstance)(box: i.InstanceBox[S[P]]) : EditorM[Expr] = 
    for {
      fc <- fromShape(box.faceComplex)
      ec <- fc.tail.traverse(new IndexedTraverse[EditorM, i.InstanceBox, ConstExpr] {
        def apply[N <: Nat](n: N)(box: i.InstanceBox[N]) : EditorM[Expr] = 
          for { mk <- attempt(box.optLabel, "Shell is incomplete") } yield mk.expr
      })
    } yield complexToExpr(ec.dim)(ec)

  // @natElim
  // def typeExpr[N <: Nat](n: N)(box: EditorBox[N]) : EditorM[Expr] = {
  //   case (Z, box) => editorSucceed(EOb(catExpr))
  //   case (S(p), box) => ???
  //     // for {
  //     //   frm <- frameComplex(box)
  //     // } yield ECell(catExpr, frm)
  // }

  //============================================================================================
  // PASTING
  //

  def onPaste(e: Expr, id: String) : EditorM[Unit] = ???
    // for {
    //   editor <- attempt(activeEditor, "No editor active")
    //   _ <- editor.pasteToCursor(e, eval(catExpr, rho), id)
    // } yield ()

  //============================================================================================
  // EXPORTING
  //

  def onExport(id: String, expr: Expr, ty: Expr) : EditorM[Unit] = {
    val (e, t) = abstractOverContext(context.toList, expr, ty)
    module.addDefinition(id, e, t)
  }

  //============================================================================================
  // IMPORTING
  //

  def onImport : EditorM[Unit] = {

    import OTTParser._

    val id = jQuery(importIdInput).value().asInstanceOf[String]
    val exprStr = jQuery(importExprInput).value().asInstanceOf[String]

    parseAll(phrase(expr), exprStr) match {
      case Success(e, _) => onPaste(e, id)
      case err => editorError("Parse error: " ++ err.toString)
    }

  }

  //============================================================================================
  // CELL ASSUMPTIONS
  //

  def onAssume: EditorM[Unit] = {

    val id = jQuery(assumeIdInput).value().asInstanceOf[String]
    val isLext = jQuery(assumeLextCheckbox).prop("checked").asInstanceOf[Boolean]

    val action = new InstanceAction[EditorM[Unit]] {

      def objectAction(i: EditorInstance)(box: i.InstanceBox[_0]) : EditorM[Unit] =
        for {
          _ <- forceNone(box.optLabel, "Selected box is already occupied!")
        } yield {

          val ty = EOb(catExpr)
          val mk = ObjectMarker(thisWksp, id, EVar(id))

          extendContext(id, ty)
          registerCell(id, EVar(id))

          box.optLabel = Some(mk)
          box.panel.refresh
          refreshEditor

          jQuery(assumeIdInput).value("")

        }

      def cellAction[P <: Nat](p: P)(i: EditorInstance)(box: i.InstanceBox[S[P]]) : EditorM[Unit] = 
        for {
          _ <- forceNone(box.optLabel, "Selected box is already occupied!")
          frmExpr <- frameExpression(i)(box)
          ty = ECell(catExpr, frmExpr)
          mk = CellMarker(p)(thisWksp, id, EVar(id))
          _ <- runCheck(
            checkT(rho, gma, ty)
          )(
            msg => editorError("Typechecking error: " ++ msg)
          )(
            _ => editorSucceed(())
          )
        } yield {

          extendContext(id, ty)
          registerCell(id, EVar(id))

          if (isLext) {
            val pId = id ++ "-is-left"
            val prop = EIsLeftExt(EVar(id))
            extendContext(pId, prop)
            registerProperty(
              LeftExtensionProperty(
                pId, EVar(pId), prop, id, EVar(id)
              )
            )
          }

          box.optLabel = Some(mk)
          box.panel.refresh
          refreshEditor

          jQuery(assumeIdInput).value("")
          jQuery(assumeLextCheckbox).prop("checked", false)

        }
    }

    runInstanceAction(action)

  }

  //============================================================================================
  // COMPOSITION
  //

  def onCompose: EditorM[Unit] = ???
    // for {
    //   editor <- attempt(activeEditor, "No editor active")
    //   _ <- editor.withSelection(new editor.BoxAction[Unit] {

    //     def objectAction(box: editor.EditorBox[_0]) : EditorM[Unit] =
    //       editorError("Cannot fill an object!")

    //     def cellAction[P <: Nat](p: P)(fillBox: editor.EditorBox[S[P]]) : EditorM[Unit] =
    //       for {
    //         fc <- fromShape(fillBox.faceComplex)
    //         (fpBoxes, compBox, nchTr) <- (
    //           fc.tail match {
    //             case Complex(fpBoxes, Box(compBox, nchTr)) => editorSucceed((fpBoxes, compBox, nchTr))
    //             case _ => editorError("Malformed composition complex")
    //           }
    //         )
    //         _ <- forceNone(fillBox.optLabel, "Filling box is occupied!")
    //         _ <- forceNone(compBox.optLabel, "Composite box is occupied!")
    //         nch <- nchTr.traverse[EditorM, Expr]({
    //           case nst => 
    //             for { 
    //               b <- attempt(nst.baseValue.optLabel, "Niche is not complete!") 
    //             } yield b.expr
    //         })

    //         // This is pretty damn ugly ...
    //         fp <- Suite.traverse[EditorM, editor.BNst, editor.ENst, P](fpBoxes)(editor.SuiteExtractExprs)

    //         comp = EComp(catExpr, fp, nch)
    //         fill = EFill(catExpr, fp, nch)
    //         prop = EFillIsLeft(catExpr, fp, nch)
    //         propTy = EIsLeftExt(fill)

    //         compId = jQuery(composeIdInput).value().asInstanceOf[String]
    //         fillId = jQuery(composeFillInput).value().asInstanceOf[String]
    //         propId = jQuery(composePropInput).value().asInstanceOf[String]

    //         // The property must be registered first, since assigning the
    //         // label will trigger a recalculation of the cell visualization
    //         _ = registerProperty(
    //           LeftExtensionProperty(
    //             propId, prop, propTy, fillId, fill
    //           )
    //         )

    //         _ = compBox.optLabel = Some(Marker(p)(thisWksp, compId, comp))
    //         _ = fillBox.optLabel = Some(Marker(S(p))(thisWksp, fillId, fill))

    //         compTy <- editor.typeExpr(p)(compBox)
    //         fillTy <- editor.typeExpr(S(p))(fillBox)

    //       } yield {

    //         extendEnvironment(compId, comp, compTy)
    //         extendEnvironment(fillId, fill, fillTy)
    //         extendEnvironment(propId, prop, propTy)

    //         registerCell(compId, comp)
    //         registerCell(fillId, fill)

    //         compBox.panel.refresh
    //         fillBox.panel.refresh
    //         editor.ce.refreshGallery

    //       }

    //   })
    // } yield ()

  //============================================================================================
  // LEFT LIFTING
  //

  def onLeftLift : EditorM[Unit] = ???
    // for {
    //   editor <- attempt(activeEditor, "No editor active")
    //   _ <- editor.liftAtSelection(new editor.LiftAction[Unit] {

    //     def apply[P <: Nat](p: P)(fillBox: editor.EditorBox[S[S[P]]]) : EditorM[Unit] = 
    //       for {
    //         fillCmplx <- fromShape(fillBox.faceComplex)
    //         liftCmplx <- fromShape(fillCmplx.sourceAt(S(p))(Nil :: Nil))
    //         evidenceCmplx <- fromShape(fillCmplx.sourceAt(S(p))((Zipper.rootAddr(p) :: Nil) :: Nil))
    //         targetCmplx <- fromShape(fillCmplx.target)
    //         competitorCmplx <- fromShape(targetCmplx.target)

    //         // Extract the required boxes
    //         liftBox = liftCmplx.headValue
    //         evidenceBox = evidenceCmplx.headValue
    //         targetBox = targetCmplx.headValue
    //         competitorBox = competitorCmplx.headValue

    //         _ <- forceNone(fillBox.optLabel, "Filling box is occupied!")
    //         _ <- forceNone(liftBox.optLabel, "Lift box is occupied!")
    //         eMk <- attempt(evidenceBox.optLabel, "Evidence box is empty!")
    //         cMk <- attempt(competitorBox.optLabel, "Competitor is empty!")
    //         tMk <- attempt(targetBox.optLabel, "Target is empty!")

    //         lextWitness <- attempt(
    //           findLeftExtensionWitness(eMk.displayName),
    //           "No left lifting property found for " ++ eMk.displayName
    //         )

    //         liftExpr = ELiftLeft(eMk.expr, lextWitness, cMk.expr, tMk.expr)
    //         fillExpr = EFillLeft(eMk.expr, lextWitness, cMk.expr, tMk.expr)

    //         liftId = jQuery(liftIdInput).value().asInstanceOf[String]
    //         fillId = jQuery(liftFillInput).value().asInstanceOf[String]
    //         lextId = jQuery(liftLextInput).value().asInstanceOf[String]
    //         rextId = jQuery(liftRextInput).value().asInstanceOf[String]

    //         lextProp = EFillLeftIsLeft(eMk.expr, lextWitness, cMk.expr, tMk.expr)
    //         lextTy = EIsLeftExt(fillExpr)

    //         _ = registerProperty(
    //           LeftExtensionProperty(
    //             lextId, lextProp, lextTy, fillId, fillExpr
    //           )
    //         )

    //         rextAddr = rbAddr(S(p))(Nil)
    //         rextProp = EFillLeftIsRight(eMk.expr, lextWitness, cMk.expr, tMk.expr, liftExpr, fillExpr, lextProp)
    //         rextTy = EIsRightExt(fillExpr, rextAddr)

    //         _ = registerProperty(
    //           RightExtensionProperty(
    //             rextId, rextProp, rextTy, rextAddr, fillId, fillExpr
    //           )
    //         )

    //         _ = liftBox.optLabel = Some(Marker(S(p))(thisWksp, liftId, liftExpr))
    //         _ = fillBox.optLabel = Some(Marker(S(S(p)))(thisWksp, fillId, fillExpr))

    //         liftTy <- editor.typeExpr(S(p))(liftBox)
    //         fillTy <- editor.typeExpr(S(S(p)))(fillBox)

    //       } yield {

    //         extendEnvironment(liftId, liftExpr, liftTy)
    //         extendEnvironment(fillId, fillExpr, fillTy)
    //         extendEnvironment(lextId, lextProp, lextTy)
    //         extendEnvironment(rextId, rextProp, rextTy)

    //         registerCell(liftId, liftExpr)
    //         registerCell(fillId, fillExpr)

    //         liftBox.panel.refresh
    //         fillBox.panel.refresh
    //         editor.ce.refreshGallery

    //       }

    //   })
    // } yield ()

  //============================================================================================
  // RIGHT LIFTING
  //

  def onRightLift : EditorM[Unit] = ???
    // for {
    //   editor <- attempt(activeEditor, "No editor active")
    //   _ <- editor.liftAtSelection(new editor.LiftAction[Unit] {

    //     // Okay, this time, we have to *find* the evidence guy and record
    //     // his address so that we can search for the appropriate lifting property...
    //     def apply[P <: Nat](p: P)(fillBox: editor.EditorBox[S[S[P]]]) : EditorM[Unit] = 
    //       for {
    //         fillCmplx <- fromShape(fillBox.faceComplex)
    //         (targetBox, tgtCanopy) <- fromShape(fillCmplx.tail.head.asFrame)
    //         (evidenceBox, liftBox) <- (
    //           tgtCanopy.nodes match {
    //             case eb :: lb :: Nil => editorSucceed((eb, lb))
    //             case _ => editorError("Malformed right lifting setup")
    //           }
    //         )

    //         // Calculate the appropriate address
    //         addr <- attempt(liftBox.nestingAddress.headOption, "Malformed address")
    //         rextEvAddr <- attempt(addr.headOption, "Malformed address")

    //         zp <- fromShape(fillCmplx.tail.tail.head.seekTo(addr))
    //         (_, cn) <- fromShape(zp._1.asFrame)
    //         competitorBox <- (
    //           cn.nodes match {
    //             case cb :: Nil => editorSucceed(cb)
    //             case _ => editorError("Malformed competitor setup")
    //           }
    //         )

    //         _ <- forceNone(fillBox.optLabel, "Filling box is occupied!")
    //         _ <- forceNone(liftBox.optLabel, "Lift box is occupied!")
    //         eMk <- attempt(evidenceBox.optLabel, "Evidence box is empty!")
    //         cMk <- attempt(competitorBox.optLabel, "Competitor is empty!")
    //         tMk <- attempt(targetBox.optLabel, "Target is empty!")

    //         rextWitness <- attempt(
    //           findRightExtensionWitness(eMk.displayName, rbAddr(p)(rextEvAddr)),
    //           "No right lifting property found for " ++ eMk.displayName
    //         )

    //         liftExpr = ELiftRight(eMk.expr, rextWitness, cMk.expr, tMk.expr)
    //         fillExpr = EFillRight(eMk.expr, rextWitness, cMk.expr, tMk.expr)

    //         liftId = jQuery(liftIdInput).value().asInstanceOf[String]
    //         fillId = jQuery(liftFillInput).value().asInstanceOf[String]
    //         lextId = jQuery(liftLextInput).value().asInstanceOf[String]
    //         rextId = jQuery(liftRextInput).value().asInstanceOf[String]

    //         lextProp = EFillRightIsLeft(eMk.expr, rextWitness, cMk.expr, tMk.expr)
    //         lextTy = EIsLeftExt(fillExpr)

    //         _ = registerProperty(
    //           LeftExtensionProperty(
    //             lextId, lextProp, lextTy, fillId, fillExpr
    //           )
    //         )

    //         rextAddr = rbAddr(S(p))(addr)
    //         rextProp = EFillLeftIsRight(eMk.expr, rextWitness, cMk.expr, tMk.expr, liftExpr, fillExpr, lextProp)
    //         rextTy = EIsRightExt(fillExpr, rextAddr)

    //         _ = registerProperty(
    //           RightExtensionProperty(
    //             rextId, rextProp, rextTy, rextAddr, fillId, fillExpr
    //           )
    //         )

    //         _ = liftBox.optLabel = Some(Marker(S(p))(thisWksp, liftId, liftExpr))
    //         _ = fillBox.optLabel = Some(Marker(S(S(p)))(thisWksp, fillId, fillExpr))

    //         liftTy <- editor.typeExpr(S(p))(liftBox)
    //         fillTy <- editor.typeExpr(S(S(p)))(fillBox)

    //       } yield {

    //         extendEnvironment(liftId, liftExpr, liftTy)
    //         extendEnvironment(fillId, fillExpr, fillTy)
    //         extendEnvironment(lextId, lextProp, lextTy)
    //         extendEnvironment(rextId, rextProp, rextTy)

    //         registerCell(liftId, liftExpr)
    //         registerCell(fillId, fillExpr)

    //         liftBox.panel.refresh
    //         fillBox.panel.refresh
    //         editor.ce.refreshGallery

    //       }

    //   })
    // } yield ()

  //============================================================================================
  // SHELL FORCING
  //

  def onShellForce : EditorM[Unit] = ???
    // for {
    //   editor <- attempt(activeEditor, "No editor active")
    //   _ <- editor.withSelection(new editor.BoxAction[Unit] {

    //     def objectAction(box: editor.EditorBox[_0]) : EditorM[Unit] =
    //       editorError("Cannot shell force an object!")

    //     def cellAction[P <: Nat](p: P)(fillBox: editor.EditorBox[S[P]]) : EditorM[Unit] =
    //       for {
    //         fc <- fromShape(fillBox.faceComplex)
    //         fillMk <- attempt(fillBox.optLabel, "Selected box is empty")
    //         fillEv <- attempt(findLeftExtensionWitness(fillMk.displayName), "Selected box is not a left extension")

    //         nst <- fc.tail.head.traverse[EditorM, (editor.EditorBox[P], Marker[P], Option[Expr])]({
    //           case b => 
    //             for {
    //               mk <- attempt(b.optLabel, "Shell is incomplete!")
    //             } yield (b, mk, findLeftExtensionWitness(mk.displayName))
    //         })

    //         ((tgtBox, tgtMk, tgtEvOpt), srcTrplTr) <- fromShape(nst.asFrame)

    //         (box, mk, src, tgt) <- (
    //           tgtEvOpt match {
    //             case None => 
    //               for {
    //                 src <- srcTrplTr.traverse[EditorM, Expr]({
    //                   case (_, m, evOpt) => attempt(evOpt, "Missing evidence for: " ++ m.displayName)
    //                 })
    //               } yield (tgtBox, tgtMk, rbTree(src), EEmpty)
    //             case Some(tgtEv) => {
    //               srcTrplTr.nodes.filterNot(_._3.isDefined) match {
    //                 case (b, m, _) :: Nil => {

    //                   val src = rbTree(
    //                     srcTrplTr map {
    //                       case (_, _, None) => EEmpty
    //                       case (_, _, Some(ev)) => ev
    //                     }
    //                   )

    //                   editorSucceed((b, m, src, tgtEv))
    //                 }
    //                 case _ => editorError("Malformed evidence tree.")
    //               }

    //             }
    //           }
    //         )
    //       } yield {

    //         val prop = LeftExtensionProperty(
    //           mk.displayName ++ "-isLeft",
    //           EShellIsLeft(fillMk.expr, fillEv, src, tgt),
    //           EIsLeftExt(mk.expr),
    //           mk.displayName,
    //           mk.expr
    //         )

    //         registerProperty(prop)

    //         box.optLabel = Some(mk)
    //         box.panel.refresh
    //         editor.ce.refreshGallery

    //       }

    //   })
    // } yield ()

}

