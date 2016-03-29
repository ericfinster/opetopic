/**
  * Prover.scala - Opetopic Theorem Prover
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import scalaz.std.string._

import opetopic._
import opetopic.tt._
import OpetopicTypeChecker._
import opetopic.js.JQuerySemanticUI._

import syntax.complex._
import syntax.tree._
import syntax.nesting._
import syntax.suite._

object Prover extends JSApp {

  def main : Unit = {

    println("Launched Opetopic Prover.")

    jQuery(".ui.accordion").accordion()
    jQuery(".menu .item").tab()
    jQuery("#new-editor").on("click", () => newEditor)
    jQuery("#shell-force-item").on("click", () => runEditorAction(onShellForce))

    jQuery("#tab-pane").keypress((e : JQueryEventObject) => {
      e.which match {
        case 101 => for { editor <- activeEditor } { editor.ce.extrudeSelection }
        case 100 => for { editor <- activeEditor } { editor.ce.extrudeDrop }
        case 112 => for { editor <- activeEditor } { editor.ce.sprout }
        case _ => ()
      }
    })

    jQuery(".ui.checkbox").checkbox()

    jQuery("#assume-form").on("submit",
      (e : JQueryEventObject) => {
        e.preventDefault
        runEditorAction(onAssume)
      })

    jQuery("#compose-form").on("submit",
      (e : JQueryEventObject) => {
        e.preventDefault
        runEditorAction(onCompose)
      })

    jQuery("#left-lift-btn").on("click", 
      (e : JQueryEventObject) => {
        e.preventDefault
        runEditorAction(onLeftLift)
      })

    jQuery("#right-lift-btn").on("click",
      (e : JQueryEventObject) => {
        e.preventDefault
        runEditorAction(onRightLift)
      })

    jQuery("#comp-id-input").on("input", () => {
      val compId = jQuery("#comp-id-input").value().asInstanceOf[String]
      jQuery("#fill-id-input").value(compId ++ "-fill")
      jQuery("#prop-id-input").value(compId ++ "-fill-isLeft")
    })

    jQuery("#lift-id-input").on("input", () => {
      val liftId = jQuery("#lift-id-input").value().asInstanceOf[String]
      jQuery("#lift-fill-id-input").value(liftId ++ "-fill")
      jQuery("#left-prop-id-input").value(liftId ++ "-fill-isLeft")
      jQuery("#right-prop-id-input").value(liftId ++ "-fill-isRight")
    })

    extendContext("X", ECat)
    newEditor

  }

  //============================================================================================
  // EDITOR ROUTINES
  //

  def runEditorAction(act: EditorM[Unit]) : Unit = {

    import scalaz.\/
    import scalaz.\/-
    import scalaz.-\/

    act match {
      case -\/(msg: String) => showErrorMessage(msg)
      case \/-(_) => ()
    }

  }

  var editorCount: Int = 0
  var activeEditor: Option[Editor] = None

  def newEditor: Unit = {

    val editor = new Editor
    editorCount += 1

    val cntStr = editorCount.toString
    val tabName = "tab-" ++ cntStr

    val tabItem = a(cls := "item", "data-tab".attr := tabName)(cntStr).render
    val tab = div(cls := "ui tab", "data-tab".attr := tabName)(
      editor.ce.element.uiElement
    ).render

    jQuery("#tab-pager").append(tabItem)
    jQuery("#tab-pane").append(tab)

    jQuery(tabItem).tab(lit(
      onVisible = (s: String) => { activeEditor = Some(editor) }
    ))

    jQuery(tabItem).click()

  }

  def showErrorMessage(str: String) : Unit = {

    val closeIcon = i(cls := "close icon").render

    val msg = div(cls := "ui negative message")(
      closeIcon,
      div(cls := "header")("Error:"),
      p(str)
    ).render

    jQuery(closeIcon).on("click", () => {
      jQuery(msg).transition(lit(
        animation = "fade",
        onComplete = () => {
          jQuery(msg).remove()
        }
      ))
    })

    jQuery("#msg-box").append(msg)

  }

  def showInfoMessage(str: String) : Unit = {

    val closeIcon = i(cls := "close icon").render

    val msg = div(cls := "ui yellow message")(
      closeIcon,
      div(cls := "header")("Info:"),
      p(str)
    ).render

    jQuery(closeIcon).on("click", () => {
      jQuery(msg).transition(lit(
        animation = "fade",
        onComplete = () => {
          jQuery(msg).remove()
        }
      ))
    })

    jQuery("#msg-box").append(msg)

  }

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

  var gma: List[(Name, TVal)] = Nil
  var rho: Rho = RNil

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
      i(cls := "dropdown icon"), id ++ " : " ++ PrettyPrinter.prettyPrint(ty)
    ).render

    def mkPasteBtn = 
      button(
        cls := "ui icon button",
        onclick := { () => runEditorAction(onPaste(EVar(id), id)) }
      )(
        i(cls := "paste icon")
      )

    def isPasteable(ty: Expr) : Boolean = 
      ty match {
        case EOb(_) => true
        case ECell(_, _) => true
        case _ => false
      }

    val content = div(cls := "content")(
      if (isPasteable(ty)) mkPasteBtn else p("No information")
    ).render

    jQuery("#context-list").append(title, content)

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
      p(PrettyPrinter.prettyPrint(expr) ++ " : " ++ PrettyPrinter.prettyPrint(ty))
    ).render

    jQuery("#environment-list").append(title, content)

  }

  def registerCell(id: String, expr: Expr) : Unit = {

    cells += ((id, expr))

    val title = div(cls := "title")(
      i(cls := "dropdown icon"), id
    ).render

    val content = div(cls := "content")(
      button(
        cls := "ui icon button",
        onclick := { () => runEditorAction(onPaste(expr, id)) }
      )(
        i(cls := "paste icon")
      )
    ).render

    jQuery("#cell-list").append(title, content)

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

    jQuery("#property-list").append(title, content)

  }

  def findLeftExtensionWitness(cid: String) : Option[Expr] = 
    properties.filter(_.cellId == cid).filter(_.isLeft).map(_.propertyExpr).headOption

  def findRightExtensionWitness(cid: String, addr: Addr) : Option[Expr] = 
    properties.filter(_.cellId == cid).filter(_.isRightAt(addr)).map(_.propertyExpr).headOption

  def hasUniversalProperty(cid: String) : Boolean = 
    properties.filter(_.cellId == cid).length > 0 

  //============================================================================================
  // CELL ASSUMPTIONS
  //

  def onAssume: EditorM[Unit] =
    for {
      editor <- attempt(activeEditor, "No active editor")
      id = jQuery("#assume-id-input").value().asInstanceOf[String]
      isLext = jQuery("#assume-is-lext-checkbox").prop("checked").asInstanceOf[Boolean]
      _ <- editor.withSelection(new editor.BoxAction[Unit] {

          def objectAction(box: editor.EditorBox[_0]) : EditorM[Unit] = 
            for {
              _ <- forceNone(box.optLabel, "Selected box is already occupied!")
            } yield {

              val ty = EOb(catExpr)
              val mk = ObjectMarker(id, EVar(id))

              extendContext(id, ty)
              registerCell(id, EVar(id))

              box.optLabel = Some(mk)
              box.panel.refresh
              editor.ce.refreshGallery

              jQuery("#assume-id-input").value("")

            }

          def cellAction[P <: Nat](p: P)(box: editor.EditorBox[S[P]]) : EditorM[Unit] = 
            for {
              _ <- forceNone(box.optLabel, "Selected box is already occupied!")
              frm <- editor.frameComplex(box)
              ty = ECell(catExpr, frm)
              mk = CellMarker(p)(id, EVar(id))
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
              editor.ce.refreshGallery

              jQuery("#assume-id-input").value("")
              jQuery("#assume-is-lext-checkbox").prop("checked", false)

            }
        })
    } yield ()

  //============================================================================================
  // COMPOSITION
  //

  def onCompose: EditorM[Unit] =
    for {
      editor <- attempt(activeEditor, "No editor active")
      _ <- editor.withSelection(new editor.BoxAction[Unit] {

        def objectAction(box: editor.EditorBox[_0]) : EditorM[Unit] =
          editorError("Cannot fill an object!")

        def cellAction[P <: Nat](p: P)(fillBox: editor.EditorBox[S[P]]) : EditorM[Unit] =
          for {
            fc <- fromShape(fillBox.faceComplex)
            (fpBoxes, compBox, nchTr) <- (
              fc.tail match {
                case Complex(fpBoxes, Box(compBox, nchTr)) => editorSucceed((fpBoxes, compBox, nchTr))
                case _ => editorError("Malformed composition complex")
              }
            )
            _ <- forceNone(fillBox.optLabel, "Filling box is occupied!")
            _ <- forceNone(compBox.optLabel, "Composite box is occupied!")
            nch <- nchTr.traverse[EditorM, Expr]({
              case nst => 
                for { 
                  b <- attempt(nst.baseValue.optLabel, "Niche is not complete!") 
                } yield b.expr
            })

            // This is pretty damn ugly ...
            fp <- Suite.traverse[EditorM, editor.BNst, editor.ENst, P](fpBoxes)(editor.SuiteExtractExprs)

            comp = EComp(catExpr, fp, nch)
            fill = EFill(catExpr, fp, nch)
            prop = EFillIsLeft(catExpr, fp, nch)
            propTy = EIsLeftExt(fill)

            compId = jQuery("#comp-id-input").value().asInstanceOf[String]
            fillId = jQuery("#fill-id-input").value().asInstanceOf[String]
            propId = jQuery("#prop-id-input").value().asInstanceOf[String]

            // The property must be registered first, since assigning the
            // label will trigger a recalculation of the cell visualization
            _ = registerProperty(
              LeftExtensionProperty(
                propId, prop, propTy, fillId, fill
              )
            )

            _ = compBox.optLabel = Some(Marker(p)(compId, comp))
            _ = fillBox.optLabel = Some(Marker(S(p))(fillId, fill))

            compTy <- editor.typeExpr(p)(compBox)
            fillTy <- editor.typeExpr(S(p))(fillBox)

          } yield {

            extendEnvironment(compId, comp, compTy)
            extendEnvironment(fillId, fill, fillTy)
            extendEnvironment(propId, prop, propTy)

            registerCell(compId, comp)
            registerCell(fillId, fill)

            compBox.panel.refresh
            fillBox.panel.refresh
            editor.ce.refreshGallery

          }

      })
    } yield ()

  //============================================================================================
  // PASTING
  //

  def onPaste(e: Expr, id: String) : EditorM[Unit] = 
    for {
      editor <- attempt(activeEditor, "No editor active")
      _ <- editor.pasteToCursor(e, eval(catExpr, rho), id)
    } yield ()

  //============================================================================================
  // LEFT LIFTING
  //

  def onLeftLift : EditorM[Unit] = 
    for {
      editor <- attempt(activeEditor, "No editor active")
      _ <- editor.liftAtSelection(new editor.LiftAction[Unit] {

        def apply[P <: Nat](p: P)(fillBox: editor.EditorBox[S[S[P]]]) : EditorM[Unit] = 
          for {
            fillCmplx <- fromShape(fillBox.faceComplex)
            liftCmplx <- fromShape(fillCmplx.sourceAt(S(p))(Nil :: Nil))
            evidenceCmplx <- fromShape(fillCmplx.sourceAt(S(p))((Zipper.rootAddr(p) :: Nil) :: Nil))
            targetCmplx <- fromShape(fillCmplx.target)
            competitorCmplx <- fromShape(targetCmplx.target)

            // Extract the required boxes
            liftBox = liftCmplx.headValue
            evidenceBox = evidenceCmplx.headValue
            targetBox = targetCmplx.headValue
            competitorBox = competitorCmplx.headValue

            _ <- forceNone(fillBox.optLabel, "Filling box is occupied!")
            _ <- forceNone(liftBox.optLabel, "Lift box is occupied!")
            eMk <- attempt(evidenceBox.optLabel, "Evidence box is empty!")
            cMk <- attempt(competitorBox.optLabel, "Competitor is empty!")
            tMk <- attempt(targetBox.optLabel, "Target is empty!")

            lextWitness <- attempt(
              findLeftExtensionWitness(eMk.displayName),
              "No left lifting property found for " ++ eMk.displayName
            )

            liftExpr = ELiftLeft(eMk.expr, lextWitness, cMk.expr, tMk.expr)
            fillExpr = EFillLeft(eMk.expr, lextWitness, cMk.expr, tMk.expr)

            liftId = jQuery("#lift-id-input").value().asInstanceOf[String]
            fillId = jQuery("#lift-fill-id-input").value().asInstanceOf[String]
            lextId = jQuery("#left-prop-id-input").value().asInstanceOf[String]
            rextId = jQuery("#right-prop-id-input").value().asInstanceOf[String]

            lextProp = EFillLeftIsLeft(eMk.expr, lextWitness, cMk.expr, tMk.expr)
            lextTy = EIsLeftExt(fillExpr)

            _ = registerProperty(
              LeftExtensionProperty(
                lextId, lextProp, lextTy, fillId, fillExpr
              )
            )

            rextAddr = rbAddr(S(p))(Nil)
            rextProp = EFillLeftIsRight(eMk.expr, lextWitness, cMk.expr, tMk.expr, liftExpr, fillExpr, lextProp)
            rextTy = EIsRightExt(fillExpr, rextAddr)

            _ = registerProperty(
              RightExtensionProperty(
                rextId, rextProp, rextTy, rextAddr, fillId, fillExpr
              )
            )

            _ = liftBox.optLabel = Some(Marker(S(p))(liftId, liftExpr))
            _ = fillBox.optLabel = Some(Marker(S(S(p)))(fillId, fillExpr))

            liftTy <- editor.typeExpr(S(p))(liftBox)
            fillTy <- editor.typeExpr(S(S(p)))(fillBox)

          } yield {

            extendEnvironment(liftId, liftExpr, liftTy)
            extendEnvironment(fillId, fillExpr, fillTy)
            extendEnvironment(lextId, lextProp, lextTy)
            extendEnvironment(rextId, rextProp, rextTy)

            registerCell(liftId, liftExpr)
            registerCell(fillId, fillExpr)

            liftBox.panel.refresh
            fillBox.panel.refresh
            editor.ce.refreshGallery

          }

      })
    } yield ()

  //============================================================================================
  // RIGHT LIFTING
  //

  def onRightLift : EditorM[Unit] = 
    for {
      editor <- attempt(activeEditor, "No editor active")
      _ <- editor.liftAtSelection(new editor.LiftAction[Unit] {

        // Okay, this time, we have to *find* the evidence guy and record
        // his address so that we can search for the appropriate lifting property...
        def apply[P <: Nat](p: P)(fillBox: editor.EditorBox[S[S[P]]]) : EditorM[Unit] = 
          for {
            fillCmplx <- fromShape(fillBox.faceComplex)
            (targetBox, tgtCanopy) <- fromShape(fillCmplx.tail.head.asFrame)
            (evidenceBox, liftBox) <- (
              tgtCanopy.nodes match {
                case eb :: lb :: Nil => editorSucceed((eb, lb))
                case _ => editorError("Malformed right lifting setup")
              }
            )

            // Calculate the appropriate address
            addr <- attempt(liftBox.nestingAddress.headOption, "Malformed address")
            rextEvAddr <- attempt(addr.headOption, "Malformed address")

            zp <- fromShape(fillCmplx.tail.tail.head.seekTo(addr))
            (_, cn) <- fromShape(zp._1.asFrame)
            competitorBox <- (
              cn.nodes match {
                case cb :: Nil => editorSucceed(cb)
                case _ => editorError("Malformed competitor setup")
              }
            )

            _ <- forceNone(fillBox.optLabel, "Filling box is occupied!")
            _ <- forceNone(liftBox.optLabel, "Lift box is occupied!")
            eMk <- attempt(evidenceBox.optLabel, "Evidence box is empty!")
            cMk <- attempt(competitorBox.optLabel, "Competitor is empty!")
            tMk <- attempt(targetBox.optLabel, "Target is empty!")

            _ = println("Searching for right extension witness at: " ++ rbAddr(p)(rextEvAddr).toString)

            rextWitness <- attempt(
              findRightExtensionWitness(eMk.displayName, rbAddr(p)(rextEvAddr)),
              "No right lifting property found for " ++ eMk.displayName
            )

            liftExpr = ELiftRight(eMk.expr, rextWitness, cMk.expr, tMk.expr)
            fillExpr = EFillRight(eMk.expr, rextWitness, cMk.expr, tMk.expr)

            liftId = jQuery("#lift-id-input").value().asInstanceOf[String]
            fillId = jQuery("#lift-fill-id-input").value().asInstanceOf[String]
            lextId = jQuery("#left-prop-id-input").value().asInstanceOf[String]
            rextId = jQuery("#right-prop-id-input").value().asInstanceOf[String]

            lextProp = EFillRightIsLeft(eMk.expr, rextWitness, cMk.expr, tMk.expr)
            lextTy = EIsLeftExt(fillExpr)

            _ = registerProperty(
              LeftExtensionProperty(
                lextId, lextProp, lextTy, fillId, fillExpr
              )
            )

            rextAddr = rbAddr(S(p))(addr)
            rextProp = EFillLeftIsRight(eMk.expr, rextWitness, cMk.expr, tMk.expr, liftExpr, fillExpr, lextProp)
            rextTy = EIsRightExt(fillExpr, rextAddr)

            _ = registerProperty(
              RightExtensionProperty(
                rextId, rextProp, rextTy, rextAddr, fillId, fillExpr
              )
            )

            _ = liftBox.optLabel = Some(Marker(S(p))(liftId, liftExpr))
            _ = fillBox.optLabel = Some(Marker(S(S(p)))(fillId, fillExpr))

            liftTy <- editor.typeExpr(S(p))(liftBox)
            fillTy <- editor.typeExpr(S(S(p)))(fillBox)

          } yield {

            extendEnvironment(liftId, liftExpr, liftTy)
            extendEnvironment(fillId, fillExpr, fillTy)
            extendEnvironment(lextId, lextProp, lextTy)
            extendEnvironment(rextId, rextProp, rextTy)

            registerCell(liftId, liftExpr)
            registerCell(fillId, fillExpr)

            liftBox.panel.refresh
            fillBox.panel.refresh
            editor.ce.refreshGallery

          }

      })
    } yield ()

  //============================================================================================
  // SHELL FORCING
  //

  def onShellForce : EditorM[Unit] = 
    for {
      editor <- attempt(activeEditor, "No editor active")
      _ <- editor.withSelection(new editor.BoxAction[Unit] {

        def objectAction(box: editor.EditorBox[_0]) : EditorM[Unit] =
          editorError("Cannot shell force an object!")

        def cellAction[P <: Nat](p: P)(fillBox: editor.EditorBox[S[P]]) : EditorM[Unit] =
          for {
            fc <- fromShape(fillBox.faceComplex)
            fillMk <- attempt(fillBox.optLabel, "Selected box is empty")
            fillEv <- attempt(findLeftExtensionWitness(fillMk.displayName), "Selected box is not a left extension")

            nst <- fc.tail.head.traverse[EditorM, (editor.EditorBox[P], Marker[P], Option[Expr])]({
              case b => 
                for {
                  mk <- attempt(b.optLabel, "Shell is incomplete!")
                } yield (b, mk, findLeftExtensionWitness(mk.displayName))
            })

            ((tgtBox, tgtMk, tgtEvOpt), srcTrplTr) <- fromShape(nst.asFrame)

            (box, mk, src, tgt) <- (
              tgtEvOpt match {
                case None => 
                  for {
                    src <- srcTrplTr.traverse[EditorM, Expr]({
                      case (_, m, evOpt) => attempt(evOpt, "Missing evidence for: " ++ m.displayName)
                    })
                  } yield (tgtBox, tgtMk, rbTree(src), EEmpty)
                case Some(tgtEv) => {
                  srcTrplTr.nodes.filterNot(_._3.isDefined) match {
                    case (b, m, _) :: Nil => {

                      val src = rbTree(
                        srcTrplTr map {
                          case (_, _, None) => EEmpty
                          case (_, _, Some(ev)) => ev
                        }
                      )

                      editorSucceed((b, m, src, tgtEv))
                    }
                    case _ => editorError("Malformed evidence tree.")
                  }

                }
              }
            )
          } yield {

            val prop = LeftExtensionProperty(
              mk.displayName ++ "-isLeft",
              EShellIsLeft(fillMk.expr, fillEv, src, tgt),
              EIsLeftExt(mk.expr),
              mk.displayName,
              mk.expr
            )

            registerProperty(prop)

            box.optLabel = Some(mk)
            box.panel.refresh
            editor.ce.refreshGallery

          }

      })
    } yield ()

}
