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

  implicit object ExprAffixable extends Affixable[Expr] {
    type ElementType = TextType
    def decoration(e: Expr) = {

      def getColor(e: Expr) : String = 
        e match {
          case EVar(_) => "#EEF0AF"
          case _ => "white"
        }

      Decoration(
        text(PrettyPrinter.prettyPrint(e)),
        getColor(e)
      )
    }
  }

  type ConstExpr[N <: Nat] = Expr
  type OptExpr[N <: Nat] = Option[Expr]

  object RemoveOpts extends IndexedMap[OptExpr, ConstExpr] {
    def apply[N <: Nat](n: N)(o: Option[Expr]) : Expr = 
      o match {
        case None => EEmpty
        case Some(e) => e
      }
  }

  val editor = CardinalEditor[ConstExpr]
  editor.onSelectAsRoot = selectBox

  type EditorBox[N <: Nat] = 
    editor.NeutralCellBox[N]

  val menuElement = 
    div(cls := "ui top attached menu")(
      a(cls := "item", onclick := { () => jQuery(sidebarElement).sidebar("toggle")})(
        i(cls := "sidebar icon"),
        "Context"
      )
    )

  val paneElement =
    div(cls := "ui basic segment")(
      editor.element.uiElement
    ).render

  val sidebarElement = 
    div(cls := "ui vertical inverted menu sidebar")(
      div(cls := "item")("Eric", div(cls := "ui label yellow")),
      div(cls := "item")("is", div(cls := "ui label red")),
      div(cls := "item")("Cool", div(cls := "ui label blue"))
    ).render

  val contextElement = 
    div(cls := "ui bottom attached segment pushable")(
      sidebarElement, 
      div(cls := "pusher")(
        paneElement
      )
    )

  val uiElement = div(tabindex := 0)(
    menuElement,
    contextElement,
    div()
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

  // def genRho(rho: Rho, gma: Gamma) : Rho =
  //   gma match {
  //     case Nil => rho
  //     case (id, _) :: gs => {
  //       val thisRho = genRho(rho, gs)
  //       UpVar(thisRho, PVar(id), Nt(Gen(lRho(thisRho), "TC#")))
  //     }
  //   }

  def assumeVariable(id: String): Unit = 
    for {
      boxsig <- currentBox
      box = boxsig.value
      lblCmplx <- box.labelComplex
    } {

      println("Trying to assume a variable named: " ++ id)

      box.optLabel match {
        case None => {

          val exprCmplx = lblCmplx.map(RemoveOpts)
          val varType : Expr = faceToFrameExpr(exprCmplx.dim)(catVar, exprCmplx)

          val gma = context.toList
          val rho = environment

          checkT(rho, gma, varType) match {
            case -\/(msg) => println("Error: " ++ msg)
            case \/-(()) => {

              (id, eval(varType, rho)) +=: context
              environment = UpVar(rho, PVar(id), Nt(Gen(lRho(rho), "TC#")))

              box.optLabel = Some(EVar(id))
              box.panel.refresh
              editor.refreshGallery
              
            }
          }

        }
        case Some(_) => println("Box is occupied")
      }

    }


  @natElim
  def doCompose[N <: Nat](n: N)(cmplx: ExprComplex[N], id: String) : Unit = {
    case (Z, cmplx, id) => println("Dimension too low to compose")
    case (S(p), cmplx @ Complex(Complex(tl, Box(EEmpty, cn)), Dot(EEmpty, _)), id) => {

      val pd = cn.map(_.baseValue)
      val comp = EComp(catVar, tl, pd)
      val fill = EFill(catVar, tl, pd)

      for {
        tgtFrm <- cmplx.target
      } {

        val compType = faceToFrameExpr(p)(catVar, tgtFrm)
        val fillType = ECell(catVar, tl >> Box(comp, cn))

        val gma = context.toList
        val rho = environment

        val res = 
          for {
            _ <- checkT(rho, gma, compType)
            _ <- check(rho, gma, comp, eval(compType, rho))
            _ <- checkT(rho, gma, fillType)
            _ <- check(rho, gma, fill, eval(fillType, rho))
            } yield ()

        res match {
          case -\/(msg) => println("Error: " ++ msg)
          case \/-(()) => {
            println("Success!")

            // Now we need to put them in the editor, re-render
            // and update the environment.
            environment = UpVar(rho, PVar(id), eval(comp, rho))
            environment = UpVar(environment, PVar(id ++ "-def"), eval(fill, environment))

            // Crap.  We need references to the boxes.
            // Also, it means that everything will be a variable, since we want to
            // hide the ugly ones in a let-declaration.

            // So we have a bit of reorganizing to do ....

            // box.optLabel = Some(EVar(id ++ "-def"))
            // box.panel.refresh
            // editor.refreshGallery

          }
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
      lblCmplx <- box.labelComplex
    } { doCompose(lblCmplx.dim)(lblCmplx.map(RemoveOpts), id) }

}
