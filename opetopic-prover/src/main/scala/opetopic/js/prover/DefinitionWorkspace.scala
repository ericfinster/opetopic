/**
  * DefinitionWorkspace.scala - A class for building OpetopicTT definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

// import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import org.scalajs.jquery._
import scalatags.JsDom.all._
// import opetopic.js.JQuerySemanticUI._
// import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import ott.OttSyntax._
import ott.TypeChecker._
import mtl._
import ui._

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
      p("No information")
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
      p(expr.toString ++ " : " + ty.toString),
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
        onclick := { () => () /* runAction(onPaste(expr, id)) */ }
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

//   def onPaste(e: Expr, id: String) : EditorM[Unit] = 
//     for {
//       i <- attempt(activeEditor, "No editor active")
//       _ <- new Paster(i).pasteToRoot(e, id)
//     } yield ()

//   class Paster(val i: EditorInstance) {

//     type BNst[N <: Nat] = Nesting[i.InstanceBox[N], N]
//     type ENst[N <: Nat] = Nesting[ConstExpr[N], N]
//     type VNst[N <: Nat] = Nesting[ConstVal[N], N]
//     type PNst[N <: Nat] = Nesting[(EditorBox[N], ConstVal[N]), N]
//     type BVPair[N <: Nat] = (BNst[N], VNst[N])

//     def pasteToRoot(e: Expr, id: String) : EditorM[Unit] = 
//       for {
//         boxSig <- attempt(i.rootBox, "Nothing selected")
//         _ <- pasteToBox(boxSig.n)(boxSig.value, e, id)
//       } yield ()

//     @natElim
//     def pasteToBox[N <: Nat](n: N)(box: i.InstanceBox[N], e: Expr, id: String) : EditorM[Unit] = {
//       case (Z, box, e, id) => 
//         for {
//           _ <- forceNone(box.optLabel, "Destination box is not empty")
//           _ <- simpleCheck(
//             check(rho, gma, e, Ob(eval(catExpr, rho)))
//           )
//         } yield {

//           val mk = ObjectMarker(thisWksp, id, e)
//           box.optLabel = Some(mk)
//           box.panel.refresh
//           i.ce.refreshGallery

//         }
//       case (S(p: P), box, e, id) => {

//         import TypeLemmas._

//         val cat = eval(catExpr, rho)

//         for {
//           _ <- forceNone(box.optLabel, "Destination box is not empty")
//           frm <- simpleCheck(
//             for {
//               pr <- inferCell(rho, gma, e)
//               (cv, frm) = pr
//               _ <- eqNf(lRho(rho), cv, cat)
//             } yield frm
//           )
//           ed = frm.dim
//           ev <- attempt(matchNatPair(ed, p), "Expression has wrong dimension")
//           vfrm = rewriteNatIn[ValComplex, Nat, P](ev)(frm)
//           fc <- fromShape(box.faceComplex)
//           zc = Suite.zip[BNst, VNst, S[P]](fc.tail, vfrm)
//           pnst <- Suite.traverse[EditorM, BVPair, PNst, S[P]](zc)(Matcher)
//         } yield {

//           // Update all the faces
//           Suite.foreach[PNst, S[P]](pnst)(Updater)

//           // Update the main cell
//           val mk = CellMarker(p)(thisWksp, id, e)
//           box.optLabel = Some(mk)
//           box.panel.refresh
//           i.ce.refreshGallery

//         }
//       }
//     }

//     object Matcher extends IndexedTraverse[EditorM, BVPair, PNst] {
//       def apply[N <: Nat](n: N)(pr: BVPair[N]) : EditorM[PNst[N]] = {

//         val l = lRho(rho)
//         val (bnst, vnst) = pr
//         val fillings: HashMap[EditorBox[N], Val] = HashMap.empty

//         fromShape(
//           Nesting.matchTraverse(bnst, vnst)({
//             case (box, v) =>
//               box.optLabel match {
//                 case None =>
//                   if (fillings.isDefinedAt(box)) {
//                     toShape(
//                       for {
//                         _ <- eqNf(l, fillings(box), v)
//                       } yield (box, v)
//                     )
//                   } else {
//                     fillings(box) = v
//                     opetopic.succeed((box, v))
//                   }
//                 case Some(mk) =>
//                   toShape(
//                     for {
//                       _ <- eqNf(l, eval(mk.expr, rho), v)
//                     } yield (box, v)
//                   )
//               }
//           })
//         )
//       }
//     }

//     object Updater extends IndexedOp[PNst] {
//       def apply[N <: Nat](n: N)(pr: PNst[N]): Unit = {
//         pr.foreach({ case (b, v) =>
//           if (b.optLabel == None) {
//             val nf = rbV(lRho(rho), v)
//             if (nfMap.isDefinedAt(nf)) {
//               val (id, e) = nfMap(nf)
//               b.optLabel = Some(Marker(n)(thisWksp, id, e))
//             } else b.optLabel = Some(Marker(n)(thisWksp, "unknown", EEmpty))
//           }
//         })
//         pr.baseValue._1.panel.refresh
//       }
//     }

//   }

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
    } yield {

      val mk = Marker(thisWksp, id, EVar(id))

      extendContext(id, tyE, tyD)
      registerCell(id, EVar(id))

      root.label = Some(mk)
      refreshEditor

      // Have to do some evaluation here
      // to get the property domain element
      if (isTgtExt && root.dim > 0) {
        val pId = id ++ "IsTgt"
        val prop = EIsTgtU(EVar(id))
        //extendContext(pId, prop, ???)
        registerProperty(
          TgtExtProperty(
            pId, EVar(pId), prop, id, EVar(id)
          )
        )
      }

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

  def onRightLift : EditorM[Unit] = {

//         for {
//           fillCmplx <- fromShape(fillBox.faceComplex)
//           (targetBox, tgtCanopy) <- fromShape(fillCmplx.tail.head.asFrame)
//           (evidenceBox, liftBox) <- (
//             tgtCanopy.nodes match {
//               case eb :: lb :: Nil => editorSucceed((eb, lb))
//               case _ => editorError("Malformed right lifting setup")
//             }
//           )

//           // Calculate the appropriate address
//           addr <- attempt(liftBox.nestingAddress.headOption, "Malformed address")
//           rextEvAddr <- attempt(addr.headOption, "Malformed address")

//           zp <- fromShape(fillCmplx.tail.tail.head.seekTo(addr))
//           (_, cn) <- fromShape(zp._1.asFrame)
//           competitorBox <- (
//             cn.nodes match {
//               case cb :: Nil => editorSucceed(cb)
//               case _ => editorError("Malformed competitor setup")
//             }
//           )

//           _ <- forceNone(fillBox.optLabel, "Filling box is occupied!")
//           _ <- forceNone(liftBox.optLabel, "Lift box is occupied!")
//           eMk <- attempt(evidenceBox.optLabel, "Evidence box is empty!")
//           cMk <- attempt(competitorBox.optLabel, "Competitor is empty!")
//           tMk <- attempt(targetBox.optLabel, "Target is empty!")

//           rextWitness <- attempt(
//             findRightExtensionWitness(eMk.displayName, rbAddr(p)(rextEvAddr)),
//             "No right lifting property found for " ++ eMk.displayName
//           )

//           liftExpr = ELiftRight(eMk.expr, rextWitness, cMk.expr, tMk.expr)
//           fillExpr = EFillRight(eMk.expr, rextWitness, cMk.expr, tMk.expr)

//           liftId = jQuery(liftIdInput).value().asInstanceOf[String]
//           fillId = jQuery(liftFillInput).value().asInstanceOf[String]
//           lextId = jQuery(liftLextInput).value().asInstanceOf[String]
//           rextId = jQuery(liftRextInput).value().asInstanceOf[String]

//           lextProp = EFillRightIsLeft(eMk.expr, rextWitness, cMk.expr, tMk.expr)
//           lextTy = EIsLeftExt(fillExpr)

//           _ = registerProperty(
//             LeftExtensionProperty(
//               lextId, lextProp, lextTy, fillId, fillExpr
//             )
//           )

//           rextAddr = rbAddr(S(p))(addr)
//           rextProp = EFillLeftIsRight(eMk.expr, rextWitness, cMk.expr, tMk.expr, liftExpr, fillExpr, lextProp)
//           rextTy = EIsRightExt(fillExpr, rextAddr)

//           _ = registerProperty(
//             RightExtensionProperty(
//               rextId, rextProp, rextTy, rextAddr, fillId, fillExpr
//             )
//           )

//           _ = liftBox.optLabel = Some(Marker(S(p))(thisWksp, liftId, liftExpr))
//           _ = fillBox.optLabel = Some(Marker(S(S(p)))(thisWksp, fillId, fillExpr))

//           liftTy <- typeExpression(S(p))(i)(liftBox)
//           fillTy <- typeExpression(S(S(p)))(i)(fillBox)

//         } yield {

//           extendEnvironment(liftId, liftExpr, liftTy)
//           extendEnvironment(fillId, fillExpr, fillTy)
//           extendEnvironment(lextId, lextProp, lextTy)
//           extendEnvironment(rextId, rextProp, rextTy)

//           registerCell(liftId, liftExpr)
//           registerCell(fillId, fillExpr)

//           liftBox.panel.refresh
//           fillBox.panel.refresh
//           i.ce.refreshGallery

//         }

  }

//   //============================================================================================
//   // SHELL FORCING
//   //

//   def onShellForce : EditorM[Unit] = {

//     val action = new InstanceAction[EditorM[Unit]] {

//       def objectAction(i: EditorInstance)(box: i.InstanceBox[_0]) : EditorM[Unit] = 
//         editorError("Cannot shell force an object")

//       def cellAction[P <: Nat](p: P)(i: EditorInstance)(fillBox: i.InstanceBox[S[P]]) : EditorM[Unit] = 
//         for {
//           fc <- fromShape(fillBox.faceComplex)
//           fillMk <- attempt(fillBox.optLabel, "Selected box is empty")
//           fillEv <- attempt(findLeftExtensionWitness(fillMk.displayName), "Selected box is not a left extension")

//           nst <- fc.tail.head.traverse[EditorM, (i.InstanceBox[P], Marker[P], Option[Expr])]({
//             case b =>
//               for {
//                 mk <- attempt(b.optLabel, "Shell is incomplete!")
//               } yield (b, mk, findLeftExtensionWitness(mk.displayName))
//           })

//           ((tgtBox, tgtMk, tgtEvOpt), srcTrplTr) <- fromShape(nst.asFrame)

//           (box, mk, src, tgt) <- (
//             tgtEvOpt match {
//               case None =>
//                 for {
//                   src <- srcTrplTr.traverse[EditorM, Expr]({
//                     case (_, m, evOpt) => attempt(evOpt, "Missing evidence for: " ++ m.displayName)
//                   })
//                 } yield (tgtBox, tgtMk, treeToExpr(p)(src), EEmpty)
//               case Some(tgtEv) => {
//                 srcTrplTr.nodes.filterNot(_._3.isDefined) match {
//                   case (b, m, _) :: Nil => {

//                     val src = treeToExpr(p)(
//                       srcTrplTr map {
//                         case (_, _, None) => EEmpty
//                         case (_, _, Some(ev)) => ev
//                       }
//                     )

//                     editorSucceed((b, m, src, tgtEv))
//                   }
//                   case _ => editorError("Malformed evidence tree.")
//                 }

//               }
//             }
//           )
//         } yield {

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

//         }

//     }

//     runInstanceAction(action)

//   }

}

