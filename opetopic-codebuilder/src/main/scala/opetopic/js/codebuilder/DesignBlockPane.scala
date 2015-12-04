/**
  * DesignBlockPane.scala - A Pane for designing your code
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

import org.scalajs.jquery._
import org.scalajs.dom
import scalatags.JsDom.all._
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{literal => lit}


import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

import scalaz.-\/
import scalaz.\/-
import scalaz.std.option._

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

  import Cell._

  var activeCell : Option[Sigma[Cell]] = None
  var activeInstance : Option[EditorInstance] = None
  var instanceCount : Int = 0
  var hotkeysEnabled : Boolean = true
  var onSidebarShown : () => Unit = () => ()

  def initialize: Unit = {

    jQuery(accordion).accordion()
    jQuery(uiElement).find(".ui.dropdown").dropdown(lit(on = "hover"))

    jQuery(formSidebar).sidebar(lit(
      context = jQuery(tabContainer),
      dimPage = false,
      closable = false,
      onShow = { () => onSidebarShown() }
    ))

    jQuery(uiElement).keypress((e : JQueryEventObject) => {
      if (hotkeysEnabled) {
        e.which match {
          case 97 => onAssumeVariable
          case 102 => onComposeDiagram
          case 101 => for { i <- activeInstance } { i.editor.extrudeSelection }
          case 100 => for { i <- activeInstance } { i.editor.extrudeDrop }
          case 112 => for { i <- activeInstance } { i.editor.sprout }
          case 108 => onLiftCell
          case 118 => for { i <- activeInstance } { i.selectionToSvg }
          case _ => ()
        }
      }
    })

    newInstance

  }

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

  def pasteToCursor: Unit = 
    for {
      cell <- activeCell
      i <- activeInstance
    }{ i.doPaste(cell.n)(cell.value) }

  def registerCell[N <: Nat](cell: Cell[N]) : Unit = {

    val item =
      div(
        cls := "item",
        onclick := { () => activeCell = Some(Sigma(cell.dim)(cell)) }
      )(
        div(cls := "content", style := "margin-left: 10px")(cell.id)
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

  def onAssumeVariable: Unit = {

    val idInput = input(`type` := "text", placeholder := "Identifier").render
    val isLeftExt = input(`type` := "checkbox", tabindex := 0, cls := "hidden").render
    // val isRightExt = input(`type` := "checkbox", tabindex := 0, cls := "hidden")

    val varForm =
      form(cls := "ui form")(
        div(cls := "inline fields")(
          div(cls := "field")(
            label("Assume Variable:"),
            idInput
          ),
          div(cls := "field")(
            div(cls := "ui checkbox")(
              isLeftExt,
              label("Left Extension")
            )
          ),
          // div(cls := "field")(
          //   div(cls := "ui checkbox")(
          //     isRightExt,
          //     label("Right Extension")
          //   )
          // ),
          button(cls := "ui button", `type` := "submit")("Ok")
        )
      ).render

    jQuery(formSidebar).empty().append(
      div(cls := "ui basic segment")(varForm).render
    )

    jQuery(varForm).find(".checkbox").checkbox()

    onSidebarShown = { () => jQuery(idInput).focus() }

    jQuery(varForm).on("submit", (e : JQueryEventObject) => { 
      e.preventDefault 
      hotkeysEnabled = true
      jQuery(formSidebar).sidebar("hide")
      jQuery(uiElement).focus
      val id = jQuery(idInput).value().asInstanceOf[String]
      val lex = jQuery(isLeftExt).prop("checked").asInstanceOf[Boolean]
      for { i <- activeInstance } { i.assumeVariable(id, lex) }
    })

    hotkeysEnabled = false
    jQuery(formSidebar).sidebar("show")

  }

  def onComposeDiagram : Unit = {

    val idInput = input(`type` := "text", placeholder := "Identifier").render

    val compForm =
      form(cls := "ui form")(
        div(cls := "inline fields")(
          div(cls := "field")(
            label("Compose Diagram:"),
            idInput
          ),
          button(cls := "ui button", `type` := "submit")("Ok")
        )
      ).render

    jQuery(formSidebar).empty().append(
      div(cls := "ui basic segment")(compForm).render
    )

    onSidebarShown = { () => jQuery(idInput).focus() }

    jQuery(compForm).on("submit", (e : JQueryEventObject) => { 
      e.preventDefault 
      hotkeysEnabled = true
      jQuery(formSidebar).sidebar("hide")
      jQuery(uiElement).focus
      val id = jQuery(idInput).value().toString
      for { i <- activeInstance } { i.composeDiagram(id) }
    })

    hotkeysEnabled = false
    jQuery(formSidebar).sidebar("show")

  }

  def onLiftCell : Unit = {

    val idInput = input(`type` := "text", placeholder := "Identifier").render

    val liftForm =
      form(cls := "ui form")(
        div(cls := "inline fields")(
          div(cls := "field")(
            label("Lift Cell:"),
            idInput
          ),
          button(cls := "ui button", `type` := "submit")("Ok")
        )
      ).render

    jQuery(formSidebar).empty().append(
      div(cls := "ui basic segment")(liftForm).render
    )

    onSidebarShown = { () => jQuery(idInput).focus() }

    jQuery(liftForm).on("submit", (e : JQueryEventObject) => { 
      e.preventDefault 
      hotkeysEnabled = true
      jQuery(formSidebar).sidebar("hide")
      jQuery(uiElement).focus
      val id = jQuery(idInput).value().toString
      for { i <- activeInstance } { i.lift(id) }
    })

    hotkeysEnabled = false
    jQuery(formSidebar).sidebar("show")

  }

  val environmentMenu = 
    div(cls := "ui large selection list").render

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

  val tabs = div(cls := "ui basic segment", style := "min-height: 310px").render
  val formSidebar = div(cls := "ui bottom sidebar", style := "z-index: 10 ; background-color: white").render

  val tabContainer = div(cls := "ui attached pushable segment", style := "overflow: hidden")(
    formSidebar,
    div(cls := "pusher")(
      tabs
    )
  ).render

  val tabLabels = 
    div(cls := "ui center aligned bottom attached segment")(
      a(cls := "ui grey circular label", onclick := { () => newInstance })("+")
    ).render

  val bottomElement = 
    div(cls := "ui raised segment builder")(
      div(cls := "ui celled grid")(
        div(cls := "three wide column")(
          accordion
        ),
        div(cls := "ten wide center aligned column")(
          div(cls := "ui top attached menu")(
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
          tabContainer,
          tabLabels
        ),
        div(cls := "three wide column")(
          p("Right column")
        )
      )
    )

  val environmentPopup = 
    div(id := "envPopup", cls := "ui vertical popup menu", style := "display: none")(
      a(cls := "item", onclick := { () => pasteToCursor })("Paste to Cursor"),
      a(cls := "item", onclick := { () => () })("Paste to New Editor"),
      a(cls := "item", onclick := { () => () })("Show Universal")
    ).render

  val uiElement = div(tabindex := 0)(
    bottomElement,
    environmentPopup
  ).render

  //============================================================================================
  // CELL AFFIXABLE INSTANCE
  //

  implicit object CellAffixableFamily extends AffixableFamily[Cell] {
    def apply[N <: Nat](n: N) : Affixable[Cell[N]] = 
      new Affixable[Cell[N]] {
        type ElementType = TextType
        def decoration(cell: Cell[N]) = 
          cell.isLeftExt match {
            case None => 
              cell.expr match {
                case EVar(_) => Decoration(text(cell.id), "variable")
                case _ => Decoration(text(cell.id), "composite")
              }
            case Some(_) => Decoration(text(cell.id), "universal")
          }
      }
  }

  //============================================================================================
  // SEMANTICS
  //

  val catPat = PVar("X")
  val catVar = EVar("X")

  val context : ListBuffer[(String, Val)] = 
    ListBuffer(("X", Cat))

  var environment : Rho = UpVar(RNil, PVar("X"), Nt(Gen(0, "TC#")))

  class EditorInstance {

    val editor = CardinalEditor[Cell]
    editor.onSelectAsRoot = onSelectAsRoot

    type EditorBox[N <: Nat] = editor.CardinalCellBox[N]

    object EditorBoxToExpr extends IndexedMap[EditorBox, ConstExpr] {
      def apply[N <: Nat](n: N)(box: EditorBox[N]) : Expr = 
        box.optLabel match {
          case Some(cell) => cell.expr
          case None => EEmpty
        }
    }

    object ExtractCells extends IndexedTraverse[Option, EditorBox, Cell] {
      def apply[N <: Nat](n: N)(box: EditorBox[N]) : Option[Cell[N]] = 
        box.optLabel
    }

    var currentBox: Option[Sigma[EditorBox]] = None

    def onSelectAsRoot(boxsig: Sigma[editor.CardinalCellBox]) : Unit = {
      currentBox = Some(boxsig)
    }

    def selectionToSvg: Unit = 
      for {
        boxsig <- currentBox
        lc <- boxsig.value.labelComplex
      } {

        val exporter = new SvgExporter(lc)

        jQuery(".ui.modal.svgexport").find("#exportlink").
          attr(lit(href = "data:text/plain;charset=utf-8," ++ js.URIUtils.encodeURIComponent(exporter.svgString)))

        jQuery(".ui.modal.svgexport").modal("show")

      }

    // You could really clean things up with more of
    // this kind of stuff ...
    class SuccBoxOps[P <: Nat](box: EditorBox[S[P]]) {

      def frameComplex : Option[ExprComplex[P]] = 
        for {
          fc <- toOpt(box.faceComplex)
        } yield fc.tail.map(EditorBoxToExpr)

      def cellComplex : Option[Complex[Cell, P]] =
        for {
          fc <- toOpt(box.faceComplex)
          res <- fc.tail.traverse(ExtractCells) 
        } yield res

    }

    // implicit def toSuccBoxOps[P <: Nat](box: EditorBox[S[P]]) : SuccBoxOps[P] = 
    //   new SuccBoxOps(box)

    //============================================================================================
    // ASSUME A VARIABLE
    //

    def assumeVariable(id: String, isLex: Boolean): Unit = 
      for {
        boxsig <- currentBox
        fc <- boxsig.value.faceComplex
      } { doAssume(boxsig.n)(fc, id, isLex) }

    @natElim
    def doAssume[N <: Nat](n: N)(cmplx: Complex[EditorBox, N], id: String, isLex: Boolean) : Unit = {
      case (Z, Complex(_, Obj(b)), id, isLex) => 
        b.optLabel match {
          case None => {

            val cell = ObjectCell(id, EVar(id))
            val rho = environment

            (id, eval(cell.ty, rho)) +=: context
            environment = UpVar(rho, PVar(id), Nt(Gen(lRho(rho), "TC#")))

            registerCell(cell)

            b.optLabel = Some(cell)
            b.panel.refresh
            editor.refreshGallery

          }
          case Some(_) => println("Cell is occupied")
        }
      case (Z, _, _, _) => println("Malformed complex")
      case (S(p: P), Complex(tl, Dot(b, _)), id, isLex) => 
        b.optLabel match {
          case None => {

            tl.traverse(ExtractCells) match {
              case Some(cellCmplx) => {

                val frmCmplx : ExprComplex[P] = tl.map(EditorBoxToExpr)
                val varType : Expr = ECell(catVar, frmCmplx)

                val gma = context.toList
                val rho = environment

                checkT(rho, gma, varType) match {
                  case -\/(msg) => println("Error: " ++ msg)
                  case \/-(()) => {

                    (id, eval(varType, rho)) +=: context
                    environment = UpVar(rho, PVar(id), Nt(Gen(lRho(rho), "TC#")))

                    val isLexOpt : Option[Expr] =
                      if (isLex) {
                        val lexId = id ++ "-is-lex"
                        val lexType = ELeftExt(EVar(id))
                        (lexId, eval(lexType, environment)) +=: context
                        environment = UpVar(environment, PVar(lexId), Nt(Gen(lRho(environment), "TC#")))
                        Some(EVar(id ++ "-is-lex"))
                      } else None

                    val cell = HigherCell[P](id, EVar(id), cellCmplx)
                    cell.isLeftExt = isLexOpt

                    registerCell(cell)

                    b.optLabel = Some(cell)
                    b.panel.refresh
                    editor.refreshGallery

                  }

                }
              }
              case None => println("There are non-full cells")
            }

          }
          case Some(_) => println("Cell is occupied")
        }
      case (S(p: P), _, _, _) => println("Malformed complex")
    }

    //============================================================================================
    // COMPOSE A DIAGRAM
    //

    def composeDiagram(id: String): Unit = 
      for {
        boxsig <- currentBox
        fc <- boxsig.value.faceComplex
      } { doCompose(boxsig.n)(fc, id) }

    @natElim
    def faceToCell[N <: Nat](n: N)(id: String, expr: Expr, face: Complex[EditorBox, N]) : Option[Cell[N]] = {
      case (Z, id, expr, _) => Some(ObjectCell(id, expr))
      case (S(p: P), id, expr, face) => {
        for {
          cellCmplx <- face.tail.traverse(ExtractCells)
        } yield HigherCell(id, expr, cellCmplx)
      }
    }

    // Rewrite this.  It's terrible.
    @natElim
    def doCompose[N <: Nat](n: N)(cmplx: Complex[EditorBox, N], id: String) : Unit = {
      case (Z, cmplx, id) => println("Dimension too low to compose")
      case (S(p: P), fillCmplx @ Complex(Complex(_, Box(compBox, bcn)), Dot(fillBox, _)), id) => {

        (compBox.optLabel, fillBox.optLabel) match {
          case (None, None) => {
            for {
              compCmplx <- fillCmplx.target
            } {

              fillCmplx.map(EditorBoxToExpr).tail match {
                case Complex(web, Box(_, cn)) => {

                  val idDef = id ++ "-def"

                  val pd : Tree[Expr, P] = cn.map(_.baseValue)

                  val comp = EComp(catVar, web, pd)
                  val fill = EFill(catVar, web, pd)
                  val fillLeftExt = EFillerLeftExt(catVar, web, pd)

                  // Type check these guys!
                  val gma = context.toList
                  val rho = environment

                  val res : G[Unit] =
                    for {
                      // Build the composition cell
                      compCell <- fromOption(
                        faceToCell(p)(id, comp, compCmplx),
                        "Composition cell has un-full faces"
                      )
                      compType = compCell.ty

                      // Check the composition cell
                      _ <- checkT(rho, gma, compType)
                      compVal = eval(compType, rho)
                      _ <- check(rho, gma, comp, compVal)

                      // We temporarily fill the cell for the purposes
                      // of generating the filling cell ....
                      _ = compBox.optLabel = Some(compCell)
                      fillCell <- fromOption(
                        faceToCell(S(p))(idDef, fill, fillCmplx),
                        "Filling cell has un-full faces"
                      )
                      fillType = fillCell.ty
                      _ = compBox.optLabel = None

                      // Checking the filling is well typed
                      _ <- checkT(rho, gma, fillType)
                      fillVal = eval(fillType, rho)
                      _ <- check(rho, gma, fill, fillVal)

                      _ = ({

                        // Give the variables a type in the context
                        (id, compVal) +=: context
                        (idDef, fillVal) +=: context

                        // And now given them an expression in the environment
                        environment = UpVar(rho, PVar(id), eval(comp, rho))
                        environment = UpVar(environment, PVar(idDef), eval(fill, environment))

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
                          } yield EFillerCompLeftExt(catVar, web, prTr)

                        compCell.isLeftExt = compLeftExtEv
                        fillCell.isLeftExt = Some(fillLeftExt)

                        compBox.optLabel = Some(compCell)
                        fillBox.optLabel = Some(fillCell)

                        registerCell(compCell)
                        registerCell(fillCell)

                        compBox.panel.refresh
                        fillBox.panel.refresh
                        editor.refreshGallery

                      })
                    } yield ()

                  res match {
                    case -\/(msg) => println("Error: " ++ msg)
                    case \/-(()) => println("Composition successful")
                  }

                }
                case _ => println("Malformed tail ...")
              }
            }
          }
          case _ => println("Boxes are not empty")
        }
      }
    }

    //============================================================================================
    // PASTING
    //

    @natElim
    def doPaste[N <: Nat](n: N)(cell: Cell[N]): Unit = {
      case (Z, cell) => {

        import TypeLemmas._

        for {
          boxsig <- currentBox
          ev <- matchNatPair(boxsig.n, Z)
          box = rewriteNatIn[EditorBox, boxsig.N, _0](ev)(boxsig.value)
        } {
          box.optLabel match {
            case None => {
              box.optLabel = Some(cell)
              box.panel.refresh
              editor.refreshGallery
            }
            case Some(_) => println("Destination box is not empty")
          }
        }
      }
      case (S(p: P), cell) => {

        import TypeLemmas._

        for {
          boxsig <- currentBox
          ev <- matchNatPair(boxsig.n, S(p))
          box = rewriteNatIn[EditorBox, boxsig.N, S[P]](ev)(boxsig.value)
          fc <- box.faceComplex
        } {
          box.optLabel match {
            case None => {

              type BNst[N <: Nat] = Nesting[EditorBox[N], N]
              type CNst[N <: Nat] = Nesting[Cell[N], N]
              type PNst[N <: Nat] = Nesting[(EditorBox[N], Cell[N]), N]
              type BCPair[N <: Nat] = (BNst[N], CNst[N])

              val zc = Suite.zip[BNst, CNst, S[S[P]]](fc, cell.face)

              object Matcher extends IndexedTraverse[Option, BCPair, PNst] {
                def apply[N <: Nat](n: N)(pr: BCPair[N]) : Option[PNst[N]] = {

                  val (bnst, cnst) = pr
                  val fillings: HashMap[EditorBox[N], Expr] = HashMap.empty

                  toOpt(
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

              object Updater extends IndexedOp[PNst] {
                def apply[N <: Nat](n: N)(pr: PNst[N]): Unit = {
                  pr.foreach({
                    case (b, c) => {
                      b.optLabel = Some(c) 
                    }
                  })
                  pr.baseValue._1.panel.refresh
                }
              }

              Suite.traverse[Option, BCPair, PNst, S[S[P]]](zc)(Matcher) match {
                case None => println("There was a mismatch")
                case Some(pnst) => {
                  Suite.foreach[PNst, S[S[P]]](pnst)(Updater)
                  editor.refreshGallery
                }
              }
            }
            case Some(_) => println("Destination box is not empty")
          }
        }
      }
    }

    def lift(id: String) : Unit = 
      for {
        boxsig <- currentBox
        fc <- boxsig.value.faceComplex
      } { doLift(boxsig.n)(fc, id) }

    @natElim
    def doLift[N <: Nat](n: N)(cmplx: Complex[EditorBox, N], id: String): Unit = {
      case (Z, _, _) => println("Cannot lift here")
      case (S(p: P), _, _) => println("Cannot lift here")
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
                        } {

                          val leftBalTy = EBal(ce, nchFrm, nch)
                          val liftTy = liftCell.ty
                          val liftFillerTy = fillCell.ty
                          val liftFillerLeftExtTy = ELeftExt(liftFiller)

                          val gma = context.toList
                          val rho = environment

                          val checkRes : G[Unit] = 
                            for {
                              _ <- checkT(rho, gma, leftBalTy)
                              leftBalVal = eval(leftBalTy, rho)
                              _ <- check(rho, gma, leftBal, leftBalVal)
                              _ <- checkT(rho, gma, liftTy)
                              liftTyVal = eval(liftTy, rho)
                              _ <- check(rho, gma, lift, liftTyVal)
                              _ <- checkT(rho, gma, liftFillerTy)
                              liftFillerTyVal = eval(liftFillerTy, rho)
                              _ <- check(rho, gma, liftFiller, liftFillerTyVal)
                              _ = (id, liftTyVal) +=: context
                              _ = (idDef, liftFillerTyVal) +=: context
                              _ = environment = UpVar(rho, PVar(id), eval(lift, rho))
                              _ = environment = UpVar(environment, PVar(idDef), eval(liftFiller, environment))
                            } yield ()

                          checkRes match {
                            case -\/(msg) => {
                              // Put these guys back, since the type check blonked
                              fillBox.optLabel = None
                              rootBox.optLabel = None
                              println("Lift failed: " ++ msg)
                            }
                            case \/-(()) => {

                              println("Lift successful")

                              registerCell(liftCell)
                              registerCell(fillCell)

                              rootBox.panel.refresh
                              fillBox.panel.refresh
                              editor.refreshGallery

                            }
                          }

                        }

                      }
                      case _ => println("Unexpected: lex cell has a bizzare type")
                    }

                  }
                  case None => println("Cell is not a left extension")
                }
              }
              case _ => println("Not a liftable position")
            }
          }
          case _ => println("Not a liftable position")
        }

        // Now, we have to extract the canopy and see that it has a particular
        // form.  It should consist of exactly two cells, one of which has the
        // left extension property set.

        // The rest should be more or less simple.  You grab the target cell and
        // use the left extension to extract a balanced witness.  From this you
        // use the appropriate deconstructors to extract the lift and the fill 
        // expressions.

      }
      case (S(S(p: P)), _, _) => println("Malformed lift")
    }

  }


}
