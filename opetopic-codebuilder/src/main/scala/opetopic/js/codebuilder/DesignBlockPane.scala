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

import opetopic._
import opetopic.ui._
import opetopic.js._
import opetopic.tt._
import opetopic.pprint._
import syntax.complex._
import syntax.cardinal._
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
        jQuery("#id-input").value("")
        jQuery(".ui.modal").modal(lit(onApprove = { (x: Any) =>
          assumeVariable(jQuery("#id-input").value().asInstanceOf[String])
        }))
        jQuery(".ui.modal").modal("show")
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
    // val box = boxsig.value.asInstanceOf[editor.NeutralCellBox[boxsig.N]]

    // for {
    //   lblCmplx <- box.labelComplex
    // } {

    //   val gallery = ActiveGallery(baseConfig, lblCmplx)(
    //     new AffixableFamily[editor.OptA] {
    //       def apply[N <: Nat](n: N) : Affixable[editor.OptA[N]] =
    //         Affixable.optionAffixable(baseConfig.spacerBounds, editor.r(n))
    //     }
    //   )

    //   val div = facePaneElement

    //   val lc = div.lastChild
    //   if (lc != null) div.removeChild(lc)

    //   div.appendChild(gallery.element.uiElement)

    }

  //============================================================================================
  // SEMANTICS
  //

  val catPat = PVar("X")
  val catVar = EVar("X")

  val context : ListBuffer[(String, Val)] = 
    ListBuffer(("X", Cat))

  def assumeVariable(id: String): Unit = 
    for {
      boxsig <- currentBox
      box = boxsig.value
      lblCmplx <- box.labelComplex
    } {

      println("Trying to assume a variable named: " ++ id)

      box.optLabel match {
        case None => {

          // So, here we extract the type.  Next we're going to
          // extend the context list and then check the type?
          val exprCmplx = lblCmplx.map(RemoveOpts)
          val varType : Expr = faceToFrameExpr(exprCmplx.dim)(catVar, exprCmplx)

          // We don't have to recheck the entire context, just that
          // this new expression has type Type.

          import scalaz.-\/
          import scalaz.\/-

          def genRho(rho: Rho, gma: Gamma) : Rho = 
            gma match {
              case Nil => rho
              case (id, _) :: gs => {
                val thisRho = genRho(rho, gs)
                UpVar(thisRho, PVar(id), Nt(Gen(lRho(thisRho), "TC#")))
              }
            }

          val curContext = context.toList
          val rho = genRho(RNil, curContext)

          checkT(rho, curContext, varType) match {
            case -\/(msg) => println("Error: " ++ msg)
            case \/-(()) => {

              (id, eval(varType, rho)) +=: context
              // println("New Context: " ++ context.toList.map(_.toString).mkString(","))
              // Last thing is to actually put that guy in the box ...

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
  def faceToFrameExpr[N <: Nat](n: N)(catExpr: Expr, cmplx: ExprComplex[N]) : Expr = {
    case (Z, catExpr, cmplx) => EOb(catExpr)
    case (S(p), catExpr, Complex(frm, cell)) => ECell(catVar, frm)
  }

}
