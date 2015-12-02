/**
  * DesignBlockPane.scala - A Pane for designing your code
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import scala.collection.mutable.ListBuffer

import scalaz.-\/
import scalaz.\/-

import opetopic._
import opetopic.ui._
import opetopic.js._
import opetopic.tt._
import opetopic.pprint._
import syntax.tree._
import syntax.complex._
import syntax.suite._
import syntax.cardinal._
import syntax.nesting._

import JsDomFramework._
import JQuerySemanticUI._
import OpetopicTypeChecker._

class DesignBlockPane {

  case class ExprMarker(
    val id: String,
    val expr: Expr,
    val ty: Expr,
    var universal: Option[Expr] = None
  ) { def isUniversal: Boolean = universal != None }

  type ConstMarker[N <: Nat] = ExprMarker
  type OptMarker[N <: Nat] = Option[ExprMarker]
  type OptMarkerCmplx[N <: Nat] = Complex[OptMarker, N]

  implicit object ExprMarkerAffixable extends Affixable[ExprMarker] {
    type ElementType = TextType
    def decoration(em: ExprMarker) : Decoration[TextType] = 
      if (em.isUniversal) {
        Decoration(text(em.id), "universal")
      } else
        em.expr match {
          case EVar(_) => Decoration(text(em.id), "variable")
          case _ => Decoration(text(em.id), "composite")
        }
  }

  object ToExprComplex extends IndexedMap[OptMarker, CstExpr] {
    def apply[N <: Nat](n: N)(om: OptMarker[N]) : Expr = 
      om match {
        case None => EEmpty
        case Some(em) => em.expr
      }
  }


  var activeMarker : Option[ExprMarker] = None
  var activeInstance : Option[EditorInstance] = None

  val environmentMenu = 
    div(cls := "ui large selection list").render

  def pasteToCursor: Unit = 
    for {
      mkr <- activeMarker
      i <- activeInstance
    }{ i.doPaste(mkr) }

  def addEnvironmentElement(mkr: ExprMarker) : Unit = {

    val id = mkr.id

    val item = 
      div(
        cls := "item",
        onclick := { () => activeMarker = Some(mkr) }
      )(
        div(cls := "content", style := "margin-left: 10px")(id)
      ).render

    jQuery(environmentMenu).append(item)

    jQuery(item).popup(lit(
      movePopup = false,
      popup = environmentPopup,
      context = jQuery(uiElement),
      hoverable = "true",
      position = "right center",
      on = "click"
    ))

  }

  var instanceCount: Int = 0

  def newInstance: Unit = {

    val instance = new EditorInstance
    instanceCount += 1

    val icStr = instanceCount.toString
    val tabName = "tab-" ++ icStr

    val tab = div(cls := "ui tab", "data-tab".attr := tabName)(
      instance.editor.element.uiElement
    ).render

    val label = 
      a(cls := "ui grey circular label", 
        onclick := { () => { jQuery(tab).tab("change tab", tabName) ; activeInstance = Some(instance) } }
      )(icStr).render

    jQuery(tabs).append(tab)
    jQuery(tabLabels).append(label)
    jQuery(tab).tab("change tab", tabName)

    activeInstance = Some(instance)

  }

  val accordion = 
    div(cls := "ui fluid vertical accordion menu")(
      div(cls := "item")(
        div(cls := "active title")(i(cls := "dropdown icon"), "Environment"),
        div(cls := "active content")(environmentMenu)
      ),
      div(cls := "item")(
        div(cls := "title")(i(cls := "dropdown icon"), "Left Extensions"),
        div(cls := "content")("Wilma")
      ),
      div(cls := "item")(
        div(cls := "title")(i(cls := "dropdown icon"), "Right Extensions"),
        div(cls := "content")("Betty")
      ),
      div(cls := "item")(
        div(cls := "title")(i(cls := "dropdown icon"), "Balanced Diagrams"),
        div(cls := "content")("Barney")
      )
    ).render

  val tabs = div().render
  val tabLabels = 
    div(cls := "ui center aligned segment")(
      a(cls := "ui grey circular label", onclick := { () => newInstance })("+")
    ).render

  val bottomElement = 
    div(cls := "ui raised segment builder")(
      div(cls := "ui celled grid")(
        div(cls := "three wide column")(
          accordion
        ),
        div(cls := "ten wide center aligned column")(
          div(cls := "ui menu")(
            div(cls := "ui dropdown item")(
              "Shape",
              i(cls := "dropdown icon"),
              div(cls := "menu")(
                a(cls := "item")("Extrude"),
                a(cls := "item")("Drop"),
                a(cls := "item")("Precompose")
              )
            ),
            div(cls := "ui dropdown item")(
              "Term",
              i(cls := "dropdown icon"),
              div(cls := "menu")(
                a(cls := "item")("Assume Variable"),
                a(cls := "item")("Compose Diagram")
              )
            )
          ),
          tabs, tabLabels
        ),
        div(cls := "three wide column")(
          p("Right column")
        )
      )
    )

  val environmentPopup = 
    div(id := "envPopup", cls := "ui vertical popup menu", style := "display: none")(
      a(cls := "item", onclick := { () => jQuery(environmentMenu).find(".ui .popup").popup("hide") ; pasteToCursor })("Paste to Cursor"),
      a(cls := "item", onclick := { () => () })("Paste to New Editor"),
      a(cls := "item", onclick := { () => () })("Show Universal")
    ).render

  val uiElement = div(tabindex := 0)(
    bottomElement,
    environmentPopup
  ).render

  jQuery(uiElement).keypress((e : JQueryEventObject) => {
    e.which match {
      case 97 => {
        jQuery("#var-input").value("")
        jQuery(".ui.modal.var").modal(lit(onApprove = { (x: Any) =>
          for { i <- activeInstance }{ i.assumeVariable(jQuery("#var-input").value().asInstanceOf[String]) }
        }))
        jQuery(".ui.modal.var").modal("show")
      }
      case 102 => {
        jQuery("#comp-input").value("")
        jQuery(".ui.modal.fill").modal(lit(onApprove = { (x: Any) =>
          for { i <- activeInstance }{ i.composeDiagram(jQuery("#comp-input").value().asInstanceOf[String]) }
        }))
        jQuery(".ui.modal.fill").modal("show")
      }
      case 101 => for { i <- activeInstance } { i.editor.extrudeSelection }
      case 100 => for { i <- activeInstance } { i.editor.extrudeDrop }
      case 112 => for { i <- activeInstance } { i.editor.sprout }
      case _ => ()
    }
  })

  //============================================================================================
  // SEMANTICS
  //

  val catPat = PVar("X")
  val catVar = EVar("X")

  val context : ListBuffer[(String, Val)] = 
    ListBuffer(("X", Cat))

  var environment : Rho = UpVar(RNil, PVar("X"), Nt(Gen(0, "TC#")))

  @natElim
  def faceToFrameExpr[N <: Nat](n: N)(catExpr: Expr, cmplx: ExprComplex[N]) : Expr = {
    case (Z, catExpr, cmplx) => EOb(catExpr)
    case (S(p), catExpr, Complex(frm, cell)) => ECell(catVar, frm)
  }

  class EditorInstance {

    val editor = CardinalEditor[ConstMarker]
    editor.onSelectAsRoot = onSelectAsRoot

    type EditorBox[N <: Nat] = editor.NeutralCellBox[N]

    var currentBox: Option[Sigma[EditorBox]] = None

    def onSelectAsRoot(boxsig: Sigma[editor.CardinalCellBox]) : Unit = {

      currentBox = Some(
        Sigma(boxsig.n)(boxsig.value.asInstanceOf[EditorBox[boxsig.N]])
      )

    }

    def assumeVariable(id: String): Unit =
      for {
        boxsig <- currentBox
        box = boxsig.value
        lblCmplx <- box.labelComplex
      } {

        println("Trying to assume a variable named: " ++ id)

        box.optLabel match {
          case None => {

            val exprCmplx = lblCmplx.map(ToExprComplex)
            val varType : Expr = faceToFrameExpr(exprCmplx.dim)(catVar, exprCmplx)

            val gma = context.toList
            val rho = environment

            checkT(rho, gma, varType) match {
              case -\/(msg) => println("Error: " ++ msg)
              case \/-(()) => {

                (id, eval(varType, rho)) +=: context
                environment = UpVar(rho, PVar(id), Nt(Gen(lRho(rho), "TC#")))

                val varMkr =
                  ExprMarker(
                    id = id,
                    expr = EVar(id),
                    ty = varType
                  )

                box.optLabel = Some(varMkr)

                box.panel.refresh
                editor.refreshGallery
                
                addEnvironmentElement(varMkr)

              }
            }

          }
          case Some(_) => println("Box is occupied")
        }

      }

    @natElim
    def doCompose[N <: Nat](n: N)(cmplx: Complex[EditorBox, N], id: String) : Unit = {
      case (Z, cmplx, id) => println("Dimension too low to compose")
      case (S(p), cmplx @ Complex(Complex(_, Box(tgtBox, cn)), Dot(cellBox, _)), id) => {

        for {
          cellMarkerCmplx <- cellBox.labelComplex
          tgtMarkerCmplx <- tgtBox.labelComplex
        } {

          cellMarkerCmplx.map(ToExprComplex).tail match {
            case Complex(tl, Box(_, cn)) => {

              val pd = cn.map(_.baseValue)
              val comp = EComp(catVar, tl, pd)
              val fill = EFill(catVar, tl, pd)
              val fillLext = EFillerLeftExt(catVar, tl, pd)

              val compType = faceToFrameExpr(p)(catVar, tgtMarkerCmplx.map(ToExprComplex))
              val fillType = ECell(catVar, tl >> Box(comp, cn))

              val gma = context.toList
              val rho = environment

              val res : G[(Val, Val)] =
                for {
                  _ <- checkT(rho, gma, compType)
                  compVal = eval(compType, rho)
                  _ <- check(rho, gma, comp, compVal)
                  _ <- checkT(rho, gma, fillType)
                  fillVal = eval(fillType, rho)
                  _ <- check(rho, gma, fill, fillVal)
                } yield (compVal, fillVal)

              res match {
                case -\/(msg) => println("Error: " ++ msg)
                case \/-((compVal, fillVal)) => {

                  println("Success!")

                  // Give the variables a type in the context
                  (id, compVal) +=: context
                  (id ++ "-def", fillVal) +=: context

                  // And now given them an expression in the environment
                  environment = UpVar(rho, PVar(id), eval(comp, rho))
                  environment = UpVar(environment, PVar(id ++ "-def"), eval(fill, environment))

                  val fillMkr =
                    ExprMarker(
                      id = id ++ "-def",
                      expr = fill,
                      ty = fillType,
                      universal = Some(fillLext)
                    )

                  val compMkr =
                    ExprMarker(
                      id = id,
                      expr = comp,
                      ty = compType
                    )

                  cellBox.optLabel = Some(fillMkr)
                  tgtBox.optLabel = Some(compMkr)

                  tgtBox.panel.refresh
                  cellBox.panel.refresh
                  editor.refreshGallery

                  addEnvironmentElement(compMkr)
                  addEnvironmentElement(fillMkr)

                }
              }

            }
            case _ => println("Internal error")
          }

        }
      }
      case (S(p), Complex(Complex(tl, _), _), id) =>
        println("Malformed composition complex ...")
    }

    def composeDiagram(id: String): Unit =
      for {
        boxsig <- currentBox
        box = boxsig.value
        nc <- box.neutralComplex
      } { doCompose(nc.dim)(nc, id) }

    def doPaste(mkr: ExprMarker): Unit = ()
      // for {
      //   boxsig <- currentBox
      // } {

      //   type D = boxsig.N
      //   val d : D = boxsig.n

      //   import TypeLemmas._

      //   mkr.ty match {

      //     case EOb(ce) => {
      //       for {
      //         ev <- matchNatPair(d, Z)
      //         box = rewriteNatIn[EditorBox, D, _0](ev)(boxsig.value)
      //       } {
      //         box.optLabel match {
      //           case None => {
      //             box.optLabel = Some(mkr)
      //             box.panel.refresh
      //             editor.refreshGallery
      //           }
      //           case Some(_) => println("Destination box is not empty")
      //         }
      //       }

      //     }
      //     case ECell(ce, fe) => {
      //       for {
      //         ev <- matchNatPair(d, fe.length)
      //         box = rewriteNatIn[EditorBox, D, S[Nat]](ev)(boxsig.value)
      //         bc <- box.faceComplex
      //       } {

      //         import scalaz.std.option._

      //         type BNst[N <: Nat] = Nesting[EditorBox[N], N]
      //         type ENst[N <: Nat] = Nesting[Expr, N]
      //         type PNst[N <: Nat] = Nesting[(EditorBox[N], Expr), N]
      //         type BEPair[N <: Nat] = (BNst[N], ENst[N])

      //         val ec = fe >> Dot(mkr.expr, fe.length)
      //         val zc = Suite.zip[BNst, ENst, S[S[Nat]]](bc, ec)

      //         // Okay, right.  We actually need the box, since we're
      //         // going to have to update.

      //         // trait IndexedTraverse[T[_], F[_ <: Nat], G[_ <: Nat]] {
      //         //   def apply[N <: Nat](n: N)(fn: F[N]) : T[G[N]]
      //         // }
              
      //         // def matchTraverse[A, B, C, N <: Nat](
      //         //   nstA : Nesting[A, N], nstB : Nesting[B, N]
      //         // )(f : (A, B) => ShapeM[C]) : ShapeM[Nesting[C, N]] = {

      //         object Matcher extends IndexedTraverse[Option, BEPair, PNst] {
      //           def apply[N <: Nat](n: N)(pr: BEPair[N]) : Option[PNst[N]] = {

      //             val (bnst, enst) = pr

      //             // BUG!!! There is a subtlety here having to do with loops: if
      //             // you try to paste a glob into a loop which is empty, the source
      //             // and target of the loop get duplicated.  But then you will see
      //             // the empty at both endpoints and think the paste is okay, even
      //             // though it should "interfere with itself".

      //             toOpt(
      //               Nesting.matchTraverse(bnst, enst)({
      //                 case (b, e) => opetopic.fail("Not done")
      //                 // case (None, e) => opetopic.succeed((None, e))
      //                 // case (Some(m), e) => {
      //                 //   if (m.expr == e)
      //                 //     succeed((Some(m), e))
      //                 //   else
      //                 //     opetopic.fail("Expression mismatch.")
      //                 // }
      //               })
      //             )
      //           }
      //         }

      //         object Updater extends IndexedOp[PNst] {
      //           def apply[N <: Nat](n: N)(pr: PNst[N]): Unit = {
      //             pr.foreach({
      //               case (b, e) => println("ok")
      //             })
      //           }
      //         }

      //         for {
      //           pnst <- Suite.traverse[Option, BEPair, PNst, S[S[Nat]]](zc)(Matcher)
      //         } {
      //           Suite.foreach[PNst, S[S[Nat]]](pnst)(Updater)
      //           editor.refreshGallery
      //         }

      //       }
      //     }
      //     case _ => ()
      //   }
      // }
  }

}
