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
import syntax.cardinal._
import syntax.nesting._

import JsDomFramework._
import JQuerySemanticUI._
import OpetopicTypeChecker._

class DesignBlockPane {

  case class ExprMarker(
    val id: String,
    val expr: Expr,
    var universal: Option[Expr] = None
  ) { def isUniversal: Boolean = universal != None }

  type ConstMarker[N <: Nat] = ExprMarker
  type OptMarker[N <: Nat] = Option[ExprMarker]

  implicit object ExprMarkerAffixable extends Affixable[ExprMarker] {
    type ElementType = TextType
    def decoration(em: ExprMarker) : Decoration[TextType] = 
      if (em.isUniversal) {
        Decoration(text(em.id), "blue")
      } else
        em.expr match {
          case EVar(_) => Decoration(text(em.id), "yellow")
          case _ => Decoration(text(em.id), "red")
        }
  }

  object ToExprComplex extends IndexedMap[OptMarker, CstExpr] {
    def apply[N <: Nat](n: N)(om: OptMarker[N]) : Expr = 
      om match {
        case None => EEmpty
        case Some(em) => em.expr
      }
  }

  val editor = CardinalEditor[ConstMarker]
  editor.onSelectAsRoot = selectBox

  type EditorBox[N <: Nat] = 
    editor.NeutralCellBox[N]

  val environmentMenu = 
    div(cls := "ui large selection list").render

  def addEnvironmentElement(id: String) : Unit = {

    val item = div(cls := "item")(
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

  val accordion = 
    div(cls := "ui fluid vertical accordion menu")(
      div(cls := "item")(
        div(cls := "active title")(i(cls := "dropdown icon"), "Environment"),
        // environmentMenu
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

  val bottomElement = 
    div(cls := "ui raised segment")(
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
          editor.element.uiElement
        ),
        div(cls := "three wide column")(
          p("Right column")
        )
      )
    )

  val environmentPopup = 
    div(cls := "ui vertical popup menu", style := "display: none")(
      a(cls := "item")("Paste to Cursor"),
      a(cls := "item")("Paste to Editor"),
      a(cls := "item")("Show Universal")
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
          assumeVariable(jQuery("#var-input").value().asInstanceOf[String])
        }))
        jQuery(".ui.modal.var").modal("show")
      }
      case 102 => {
        jQuery("#comp-input").value("")
        jQuery(".ui.modal.fill").modal(lit(onApprove = { (x: Any) =>
          composeDiagram(jQuery("#comp-input").value().asInstanceOf[String])
        }))
        jQuery(".ui.modal.fill").modal("show")
      }
      case 101 => editor.extrudeSelection
      case 100 => editor.extrudeDrop
      case 112 => editor.sprout
      case _ => ()
    }
  })

  var currentBox: Option[Sigma[EditorBox]] = None

  def selectBox(boxsig: Sigma[editor.CardinalCellBox]) : Unit = {

    currentBox = Some(
      Sigma(boxsig.n)(boxsig.value.asInstanceOf[EditorBox[boxsig.N]])
    )

  }

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

              box.optLabel = Some(ExprMarker(
                id = id,
                expr = EVar(id)
              ))

              box.panel.refresh
              editor.refreshGallery
              
              addEnvironmentElement(id)

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

                cellBox.optLabel = Some(
                  ExprMarker(
                    id = id ++ "-def",
                    expr = fill,
                    universal = Some(fillLext)
                  )
                )

                tgtBox.optLabel = Some(
                  ExprMarker(
                    id = id,
                    expr = comp                    
                  )
                )

                tgtBox.panel.refresh
                cellBox.panel.refresh
                editor.refreshGallery

                addEnvironmentElement(id)
                addEnvironmentElement(id ++ "-def")

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

}
