/**
  * DefinitionWorkspace.scala - A class for building OpetopicTT definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scala.collection.mutable.ListBuffer

import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic._
import ott.OttSyntax._
import ott.OttPPrint._
import ott.TypeChecker._
import mtl._
import ui._

import Prover.runExcept

class DefinitionWorkspace(val module: Module) extends DefinitionWorkspaceUI { thisWksp =>

  //============================================================================================
  // UTILITIES
  //

  def treeToExp[A](t: STree[A])(w: A => TValT): TreeT =
    t match {
      case SLeaf => TLf()
      case SNode(a, sh) => TNd(w(a), treeToExp(sh)((b: STree[A]) => VTree(treeToExp(b)(w))))
    }

  def nstToExp(nst: SNesting[ExpT]): NstT =
    nst match {
      case SDot(e) => TDot(e)
      case SBox(e, cn) => TBox(e, treeToExp(cn)((n: SNesting[ExpT]) => VNst(nstToExp(n))))
    }

  def cmplxToExp(cmplx: SComplex[ExpT]): List[NstT] =
    Suite.SuiteTraverse.toList(cmplx).map(nstToExp(_))

  def addrToExp(addr: SAddr): AddrT =
    TAddr(addr.map((d: SDir) => addrToExp(d.dir)))

  def typechecker[A](m: TCM[A]): Except[A] =
    m.run(tcEnv)

  //============================================================================================
  // CONTEXT MANAGEMENT
  //

  // Map a normal form back to a name and an expression
  //val nfMap: HashMap[Expr, (String, Expr)] = HashMap()

  val catExp: ExpT = EVar("X")

  // Start with a category variable
  var tcEnv: TCEnv = TCEnv(List(("X", CatD)), UpVar(RNil, PVar("X"), VarD(0)))

  val context: ListBuffer[(String, ExpT)] = ListBuffer(("X", ECat()))
  val environment: ListBuffer[(String, ExpT, ExpT)] = ListBuffer()
  val cells: ListBuffer[(String, ExpT)] = ListBuffer()
  val properties: ListBuffer[Property] = ListBuffer()

//   def abstractOverContext(gma: List[(String, Expr)], expr: Expr, exprTy: Expr) : (Expr, Expr) = 
//     gma match {
//       case Nil => (expr, exprTy)
//       case (id, ty) :: gs => {
//         val (e, t) = abstractOverContext(gs, expr, exprTy)
//         (ELam(PVar(id), e), EPi(PVar(id), ty, t))
//       }
//     }

  def extendContext(id: String, tyE: ExpT, tyD: Dom) : Unit = {

    tcEnv = withVar(PVar(id), tyD)(tcEnv)
    context += ((id, tyE))

    val title = div(cls := "title")(
      i(cls := "dropdown icon"), id + " : X" 
    ).render

    val content = div(cls := "content")(
      p(show(tyE))
    ).render

    jQuery(contextList).append(title, content)

  }

  def extendEnvironment(id: String, expr: ExpT, ty: ExpT) : Unit = {

    environment += ((id, expr, ty))

//     // Update the normal form map
//     val nf = rbV(lRho(rho), eval(expr, rho))
//     nfMap(nf) = (id, expr)

    val title = div(cls := "title")(
      i(cls := "dropdown icon"), id
    ).render

    val content = div(cls := "content")(
      p(show(expr) ++ " : " + show(ty)),
      button(cls := "ui icon button", onclick := { () => () /* runAction(onExport(id, expr, ty)) */ })(
        i(cls := "check circle icon")
      )
    ).render

    jQuery(environmentList).append(title, content)

  }

  def registerCell(id: String, expr: ExpT) : Unit = {

    cells += ((id, expr))

    val title = div(cls := "title")(
      i(cls := "dropdown icon"), id
    ).render

    val content = div(cls := "content")(
      button(
        cls := "ui icon button",
        onclick := { () => runExcept(onPaste(expr, id)) }
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
        case l : TgtExtProperty => List(p("Target cell: " ++ prop.cellId))
        case r : SrcExtProperty => List(
          p("Target cell: " ++ prop.cellId),
          p("Address: " ++ r.addr.toString)
        )
      }
        
    ).render

    jQuery(propertyList).append(title, content)

  }

  def findTgtExtWitness(cid: String) : Option[ExpT] = 
    properties.filter(_.cellId == cid).filter(_.isTarget).map(_.propertyExp).headOption

  def findSrcExtWitness(cid: String, addr: SAddr) : Option[ExpT] = 
    properties.filter(_.cellId == cid).filter(_.isSourceAt(addr)).map(_.propertyExp).headOption

  def hasUniversalProperty(cid: String) : Boolean = 
    properties.filter(_.cellId == cid).length > 0 

  //============================================================================================
  // TYPING HELPERS
  //

  def frameExpression(cell: StableCell) : Except[List[NstT]] =
    for {
      face <- attempt(cell.face, "Error getting face")
      expFace <- attempt(face.traverseComplex(mk => mk.map(_.expr)), "Face not full")
      expFrm <- attempt(expFace.tail, "Selection was an object")
    } yield cmplxToExp(expFrm)

  def typeExpression(cell: StableCell): Except[ExpT] =
    for {
      face <- attempt(cell.face, "Error getting face")
      res <- face match {
        case ||(SDot(_)) => succeed[ExpT](EObj(catExp))
        case tl >> _ => 
          for {
            expFrm <- attempt(tl.traverseComplex(_.map(_.expr)), "Frame is not full")
          } yield ECell(catExp, cmplxToExp(expFrm))
      }
    } yield res

  //============================================================================================
  // PASTING
  //

  // This simplified pasting algorithm requires that the
  // frame be full and simply checks against the type...
  def onPaste(e: ExpT, id: String): Except[Unit] = {

    for {
      tab <- attempt(activeTab, "No active tab")
      pasteBox <- attempt(tab.editor.selectionRoot, "Nothing selected")
      _ <- forceNone(pasteBox.label, "Destination box is not empty")
      pasteTy <- typeExpression(pasteBox)
      _ <- typechecker(
        for {
          pasteT <- check(pasteTy, TypeD)
          pasteD <- tcEval(pasteT)
          eT <- check(e, pasteD)
        } yield ()
      )
    } yield {

      val mk = Marker(thisWksp, id, e)
      pasteBox.label = Some(mk)
      refreshEditor

    }

  }

  //============================================================================================
  // EXPORTING
  //

//   def onExport(id: String, expr: Expr, ty: Expr) : EditorM[Unit] = {
//     val (e, t) = abstractOverContext(context.toList, expr, ty)
//     module.addDefinition(id, e, t)
//   }

  //============================================================================================
  // IMPORTING
  //

//   def onImportProperty: EditorM[Unit] = {

//     import OTTParser._

//     val id = jQuery(importPropIdInput).value().asInstanceOf[String]
//     val exprStr = jQuery(importPropExprInput).value().asInstanceOf[String]

//     parseAll(phrase(expr), exprStr) match {
//       case Success(e, _) => {

//         runInstanceAction(new InstanceAction[EditorM[Unit]] {

//           def objectAction(i: EditorInstance)(box: i.InstanceBox[_0]) : EditorM[Unit] =
//             editorError("Objects don't have properties")
          
//           def cellAction[P <: Nat](p: P)(i: EditorInstance)(box: i.InstanceBox[S[P]]) : EditorM[Unit] =
//             for {
//               mk <- attempt(box.optLabel, "Cannot assign property to empty box")
//               ee = eval(mk.expr, rho)

//               _ <- simpleCheck(
//                 check(rho, gma, e, IsLeftExt(ee))
//               )  
//             } yield {

//               val propId = id
//               val propExpr = e
//               val propTy = EIsLeftExt(mk.expr)

//               registerProperty(
//                 LeftExtensionProperty(
//                   propId, propExpr, propTy, mk.displayName, mk.expr
//                 )
//               )

//               box.optLabel = Some(mk)
//               box.panel.refresh
//               refreshEditor

//             }

//         })

//       }
//       case err => editorError("Parse error: " ++ err.toString)
//     }

//   }

//   def onImportCell : EditorM[Unit] = {

//     import OTTParser._

//     val id = jQuery(importIdInput).value().asInstanceOf[String]
//     val exprStr = jQuery(importExprInput).value().asInstanceOf[String]

//     parseAll(phrase(expr), exprStr) match {
//       case Success(e, _) => {

//         for {
//           _ <- onPaste(e, id)
//         } yield {
//           val nf = rbV(lRho(rho), eval(e, rho))
//           nfMap(nf) = (id, e)
//         }

//       }
//       case err => editorError("Parse error: " ++ err.toString)
//     }

//   }

  //============================================================================================
  // CELL ASSUMPTIONS
  //

  def onAssume: Except[Unit] = {

    val id = jQuery(assumeIdInput).value().asInstanceOf[String]
    val isTgtExt = jQuery(assumeTgtExtCheckbox).prop("checked").asInstanceOf[Boolean]

    println("Going to assume new identifier: " + id)

    for {
      tab <- attempt(activeTab, "No active tab")
      root <- attempt(tab.editor.selectionRoot, "Nothing selected")
      _ <- forceNone(root.label, "Cell is occupied")
      tyE <- typeExpression(root)
      tyD <- typechecker(
        for {
          tyT <- check(tyE, TypeD)
          tyD <- tcEval(tyT)
        } yield tyD
      )

      _ = extendContext(id, tyE, tyD)
      _ = registerCell(id, EVar(id))

      _ <- if (isTgtExt && root.dim > 0) {

        val pId = id ++ "IsTgt"
        val prop = EIsTgtU(EVar(id))

        typechecker(
            for {
              propT <- check(prop, TypeD)
              propD <- tcEval(propT)
            } yield propD
        ).map(propD => {

          extendContext(pId, prop, propD)

          registerProperty(
            TgtExtProperty(
              pId, EVar(pId), prop, id, EVar(id)
            )
          )

        })

      } else succeed[Unit](())

    } yield {

      root.label = Some(Marker(thisWksp, id, EVar(id)))

      refreshEditor

      jQuery(assumeIdInput).value("")
      jQuery(assumeTgtExtCheckbox).prop("checked", false)

    }

  }

  //============================================================================================
  // COMPOSITION
  //

  def onCompose: Except[Unit] = {

    for {
      tab <- attempt(activeTab, "No active tab")
      fillBox <- attempt(tab.editor.selectionRoot, "Nothing selected")
      _ <- verify(fillBox.dim > 0, "Cannot fill an object")
      _ <- forceNone(fillBox.label, "Filling cell is occupied")
      face <- attempt(fillBox.boxFace, "Error calculating face")
      tail <- attempt(face.tail, "Box face has no tail")
      frmInfo <- attempt(tail.asFrame, "Box face has malformed frame")
      (srcs, compBox) = frmInfo
      _ <- forceNone(compBox.label, "Composition cell is occupied.")
      pdE <- attempt(srcs.traverse(_.label.map(_.expr)), "Niche is incomplete.")
      pdTr = treeToExp(pdE)(VExp(_))

      fillInfo <- (
        if (pdE.isLeaf) {
          for {
            loop <- attempt(tail.sourceAt(Nil), "Failed to calculate loop source")
            loopTail <- attempt(loop.tail, "Loop complex has no tail")
            exp <- attempt(loopTail.topValue.label.map(_.expr), "Loop source is unoccupied")
          } yield {
            (ERefl(exp), EDrop(exp), EDropIsTgt(exp), EIsTgtU(EDrop(exp)))
          }
        } else succeed[(ExpT, ExpT, ExpT, ExpT)](
          EComp(pdTr), EFill(pdTr), EFillIsTgt(pdTr), EIsTgtU(EFill(pdTr))
        )
      )

      (comp, fill, prop, propTy) = fillInfo

      compId = jQuery(composeIdInput).value().asInstanceOf[String]
      fillId = jQuery(composeFillInput).value().asInstanceOf[String]
      propId = jQuery(composePropInput).value().asInstanceOf[String]

      // The property must be registered first, since assigning the
      // label will trigger a recalculation of the cell visualization
      _ = registerProperty(
        TgtExtProperty(
          propId, prop, propTy, fillId, fill
        )
      )

      _ = compBox.label = Some(Marker(thisWksp, compId, comp))
      _ = fillBox.label = Some(Marker(thisWksp, fillId, fill))

      compTy <- typeExpression(compBox)
      fillTy <- typeExpression(fillBox)

    } yield {

      extendEnvironment(compId, comp, compTy)
      extendEnvironment(fillId, fill, fillTy)
      extendEnvironment(propId, prop, propTy)

      registerCell(compId, comp)
      registerCell(fillId, fill)

      refreshEditor

    }

  }

  //============================================================================================
  // TARGET LIFTING
  //

  // BUG! We don't actually check that there are exactly
  // two cells in the source tree of the filling cell. This
  // should be done as it is below for the source extension.

  def onTargetLift: Except[Unit] = {

    val liftAddr : FaceAddr = PrevDim(ThisDim(List(SDir(Nil))))
    val evAddr : FaceAddr = PrevDim(ThisDim(List(SDir(List(SDir(Nil))))))

    for {

      tab <- attempt(activeTab, "No active tab")
      fillBox <- attempt(tab.editor.selectionRoot, "Nothing selected")
      fillCmplx <- attempt(fillBox.boxFace, "Face calculation failed")
      liftCmplx <- attempt(fillCmplx.face(liftAddr), "Source calculation failed")
      evidenceCmplx <- attempt(fillCmplx.face(evAddr), "Evidence calculation failed")
      targetCmplx <- attempt(fillCmplx.target, "Target calculation failed")
      competitorCmplx <- attempt(targetCmplx.target, "Competitor calculation faile")

      // Extract the required boxes
      liftBox = liftCmplx.topValue
      evidenceBox = evidenceCmplx.topValue
      targetBox = targetCmplx.topValue
      competitorBox = competitorCmplx.topValue

      _ <- forceNone(fillBox.label, "Filling box is occupied!")
      _ <- forceNone(liftBox.label, "Lift box is occupied!")
      eMk <- attempt(evidenceBox.label, "Evidence box is empty!")
      cMk <- attempt(competitorBox.label, "Competitor is empty!")
      tMk <- attempt(targetBox.label, "Target is empty!")

      // _ = println("Evidence is: " + eMk.displayName)
      // _ = println("Competitor is: " + cMk.displayName)
      // _ = println("Target is: " + tMk.displayName)

      tgtExtWitness <- attempt(
        findTgtExtWitness(eMk.displayName),
        "No left lifting property found for " ++ eMk.displayName
      )

      liftExpr = ELiftTgt(eMk.expr, tgtExtWitness, cMk.expr, tMk.expr)
      fillExpr = EFillTgt(eMk.expr, tgtExtWitness, cMk.expr, tMk.expr)

      liftId = jQuery(liftIdInput).value().asInstanceOf[String]
      fillId = jQuery(liftFillInput).value().asInstanceOf[String]
      tgtPropId = jQuery(liftTgtInput).value().asInstanceOf[String]
      srcPropId = jQuery(liftSrcInput).value().asInstanceOf[String]

      tgtProp = EFillTgtIsTgt(eMk.expr, tgtExtWitness, cMk.expr, tMk.expr)
      tgtPropTy = EIsTgtU(fillExpr)

      _ = registerProperty(
        TgtExtProperty(
          tgtPropId, tgtProp, tgtPropTy, fillId, fillExpr
        )
      )

      srcPropAddr = addrToExp(Nil)
      srcProp = EFillTgtIsSrc(eMk.expr, tgtExtWitness, cMk.expr, tMk.expr)
      srcPropTy = EIsSrcU(fillExpr, srcPropAddr)

      _ = registerProperty(
        SrcExtProperty(
          srcPropId, srcProp, srcPropTy, Nil, fillId, fillExpr
        )
      )

      _ = liftBox.label = Some(Marker(thisWksp, liftId, liftExpr))
      _ = fillBox.label = Some(Marker(thisWksp, fillId, fillExpr))

      liftTy <- typeExpression(liftBox)
      fillTy <- typeExpression(fillBox)

    } yield {

      extendEnvironment(liftId, liftExpr, liftTy)
      extendEnvironment(fillId, fillExpr, fillTy)
      extendEnvironment(tgtPropId, tgtProp, tgtPropTy)
      extendEnvironment(srcPropId, srcProp, srcPropTy)

      registerCell(liftId, liftExpr)
      registerCell(fillId, fillExpr)

      refreshEditor

    }

  }

  //============================================================================================
  // RIGHT LIFTING
  //

  def onSourceLift : Except[Unit] = {

    for {

      tab <- attempt(activeTab, "No active tab")
      fillBox <- attempt(tab.editor.selectionRoot, "Nothing selected")
      fillCmplx <- attempt(fillBox.boxFace, "Face calculation failed")
      fillTail <- attempt(fillCmplx.tail, "Face has no tail")
      frmInfo <- attempt(fillTail.asFrame, "Malformed frame")
      (srcs, targetBox) = frmInfo

      boxes <- (
        srcs.toList match {
          case eb :: lb :: Nil => succeed[(StableCell, StableCell)](eb, lb)
          case _ => throwError("Malformed lift canopy")
        }
      )

      (evidenceBox, liftBox) = boxes

      addr <- attempt(liftBox.address.headOption.map(_.dir), "Malformed address")
      compAddr = PrevDim(PrevDim(ThisDim(SDir(Nil) :: addr)))

      competitorBox <- attempt(fillCmplx.elementAt(compAddr), "Failed to find competitor")

      _ <- forceNone(fillBox.label, "Filling box is occupied!")
      _ <- forceNone(liftBox.label, "Lift box is occupied!")
      eMk <- attempt(evidenceBox.label, "Evidence box is empty!")
      cMk <- attempt(competitorBox.label, "Competitor is empty!")
      tMk <- attempt(targetBox.label, "Target is empty!")

      // _ = println("Evidence is: " + eMk.displayName)
      // _ = println("Competitor is: " + cMk.displayName)
      // _ = println("Target is: " + tMk.displayName)

      // Uhhh, check the address setup here
      srcExtWitness <- attempt(
        findSrcExtWitness(eMk.displayName, addr.head.dir),
        "No right lifting property found for " ++ eMk.displayName
      )

      liftExpr = ELiftSrc(eMk.expr, srcExtWitness, cMk.expr, tMk.expr)
      fillExpr = EFillSrc(eMk.expr, srcExtWitness, cMk.expr, tMk.expr)

      liftId = jQuery(liftIdInput).value().asInstanceOf[String]
      fillId = jQuery(liftFillInput).value().asInstanceOf[String]
      tgtPropId = jQuery(liftTgtInput).value().asInstanceOf[String]
      srcPropId = jQuery(liftSrcInput).value().asInstanceOf[String]

      tgtProp = EFillSrcIsTgt(eMk.expr, srcExtWitness, cMk.expr, tMk.expr)
      tgtPropTy = EIsTgtU(fillExpr)

      _ = registerProperty(
        TgtExtProperty(
          tgtPropId, tgtProp, tgtPropTy, fillId, fillExpr
        )
      )

      srcPropAddr <- attempt(addr.headOption.map(_.dir), "Malformed source address")
      srcProp = EFillSrcIsSrc(eMk.expr, srcExtWitness, cMk.expr, tMk.expr)
      srcPropTy = EIsSrcU(fillExpr, addrToExp(srcPropAddr))

      _ = registerProperty(
        SrcExtProperty(
          srcPropId, srcProp, srcPropTy, Nil, fillId, fillExpr
        )
      )

      _ = liftBox.label = Some(Marker(thisWksp, liftId, liftExpr))
      _ = fillBox.label = Some(Marker(thisWksp, fillId, fillExpr))

      liftTy <- typeExpression(liftBox)
      fillTy <- typeExpression(fillBox)

    } yield {

      extendEnvironment(liftId, liftExpr, liftTy)
      extendEnvironment(fillId, fillExpr, fillTy)
      extendEnvironment(tgtPropId, tgtProp, tgtPropTy)
      extendEnvironment(srcPropId, srcProp, srcPropTy)

      registerCell(liftId, liftExpr)
      registerCell(fillId, fillExpr)

      refreshEditor

    }

  }

  //============================================================================================
  // TARGET CLOSURE
  //

  def onTargetClosure: Except[Unit] = {

    for {

      tab <- attempt(activeTab, "No active tab")
      fillBox <- attempt(tab.editor.selectionRoot, "Nothing selected")
      fillMk <- attempt(fillBox.label, "Selected box is empty")
      fillEv <- attempt(
        findTgtExtWitness(fillMk.displayName),
        "Could not find target evidence for " + fillMk.displayName
      )

      fillCmplx <- attempt(fillBox.boxFace, "Face calculation failed")
      _ <- attempt(fillCmplx.traverseComplex(mk => mk.label), "Frame is incomplete")

      fillTail <- attempt(fillCmplx.tail, "Face has no tail")
      frmInfo <- attempt(fillTail.asFrame, "Malformed frame")
      (srcs, targetBox) = frmInfo

      srcEvOpts <- attempt(
        srcs.traverse(srcBox => {
          srcBox.label.map(srcMk => {
            (srcBox, srcMk, findTgtExtWitness(srcMk.displayName))
          })
        }),
        "Error extracting source evidence"
      )

      tgtEvOpt = targetBox.label.flatMap(mk => {
        findTgtExtWitness(mk.displayName)
      })

      res <- tgtEvOpt match {
        case None => attempt(
          srcEvOpts.traverse({
            case (srcBox, srcMk, srcEvOpt) => srcEvOpt
          }),
          "Missing shell evidence"
        ).map(srcTr => (targetBox, srcTr))
        case Some(tgtEv) =>
          srcEvOpts.toList.filterNot(_._3.isDefined) match {
            case (bx, mk, ev) :: Nil => {

//                     val src = treeToExpr(p)(
//                       srcTrplTr map {
//                         case (_, _, None) => EEmpty
//                         case (_, _, Some(ev)) => ev
//                       }
//                     )

//                     editorSucceed((b, m, src, tgtEv))
              ???

            }
            case _ => throwError("Malformed evidence tree")
          }
      }

    } yield {

//           val propId = mk.displayName ++ "IsLeft"
//           val propExpr = EShellIsLeft(fillMk.expr, fillEv, src, tgt)
//           val propTy = EIsLeftExt(mk.expr)

//           val prop = LeftExtensionProperty(
//             propId, propExpr, propTy,
//             mk.displayName,
//             mk.expr
//           )

//           extendEnvironment(propId, propExpr, propTy)
//           registerProperty(prop)

//           box.optLabel = Some(mk)
//           box.panel.refresh
//           i.ce.refreshGallery

      println("ok")

    }

  }

}

