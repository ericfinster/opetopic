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

  sealed trait Cell[N <: Nat] {
    def id: String
    def face: Complex[Cell, N]
    def expr: Expr
    def ty: Expr
    def dim: N
    var isLeftExt: Option[Expr] = None
    var isRightExt: Option[Expr] = None
  }

  case class ObjectCell(
    val id: String,
    val expr: Expr
  ) extends Cell[_0] {
    val face = Complex[Cell] >> Obj(this)
    val ty = EOb(catVar)
    val dim = Z
  }

  case class HigherCell[P <: Nat](
    val id: String,
    val expr: Expr,
    val frm: Complex[Cell, P]
  ) extends Cell[S[P]] {
    val dim = frm.length
    val face = frm >> Dot(this, dim)
    val ty = ECell(catVar, frm.map(CellToExpr))
  }

  object Cell {

    type CNst[N <: Nat] = Nesting[Cell[N], N]

    @natElim
    def apply[N <: Nat](n: N)(id: String, expr: Expr, suite: Suite[CNst, N]) : Cell[N] = {
      case (Z, id, expr, _) => ObjectCell(id, expr)
      case (S(p: P), id, expr, suite) => HigherCell[P](id, expr, suite)
    }

  }

  object CellToExpr extends IndexedMap[Cell, ConstExpr] {
    def apply[N <: Nat](n: N)(cell: Cell[N]) : Expr = 
      cell.expr
  }

  type OptCell[N <: Nat] = Option[Cell[N]]
  type OptCellCmplx[N <: Nat] = Complex[OptCell, N]

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

  var activeCell : Option[Sigma[Cell]] = None
  var activeInstance : Option[EditorInstance] = None

  val environmentMenu = 
    div(cls := "ui large selection list").render

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
      a(cls := "item", onclick := { () => pasteToCursor })("Paste to Cursor"),
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

    //============================================================================================
    // ASSUME A VARIABLE
    //

    def assumeVariable(id: String): Unit = 
      for {
        boxsig <- currentBox
        fc <- boxsig.value.faceComplex
      } { doAssume(boxsig.n)(fc, id) }

    @natElim
    def doAssume[N <: Nat](n: N)(cmplx: Complex[EditorBox, N], id: String) : Unit = {
      case (Z, Complex(_, Obj(b)), id) => 
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
      case (Z, _, _) => println("Malformed complex")
      case (S(p: P), Complex(tl, Dot(b, _)), id) => 
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

                    val cell = HigherCell[P](id, EVar(id), cellCmplx)

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
      case (S(p: P), _, _) => println("Malformed complex")
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

    @natElim
    def doCompose[N <: Nat](n: N)(cmplx: Complex[EditorBox, N], id: String) : Unit = {
      case (Z, cmplx, id) => println("Dimension too low to compose")
      case (S(p: P), fillCmplx @ Complex(Complex(_, Box(compBox, _)), Dot(fillBox, _)), id) => {

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

              // trait IndexedTraverse[T[_], F[_ <: Nat], G[_ <: Nat]] {
              //   def apply[N <: Nat](n: N)(fn: F[N]) : T[G[N]]
              // }
              
              // def matchTraverse[A, B, C, N <: Nat](
              //   nstA : Nesting[A, N], nstB : Nesting[B, N]
              // )(f : (A, B) => ShapeM[C]) : ShapeM[Nesting[C, N]] = {

              object Matcher extends IndexedTraverse[Option, BCPair, PNst] {
                def apply[N <: Nat](n: N)(pr: BCPair[N]) : Option[PNst[N]] = {

                  val (bnst, cnst) = pr

                  // BUG!!! There is a subtlety here having to do with loops: if
                  // you try to paste a glob into a loop which is empty, the source
                  // and target of the loop get duplicated.  But then you will see
                  // the empty at both endpoints and think the paste is okay, even
                  // though it should "interfere with itself".

                  toOpt(
                    Nesting.matchTraverse(bnst, cnst)({
                      case (b, c) =>
                        b.optLabel match {
                          case None => opetopic.succeed((b, c))
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
  }

}
