/**
  * ContextStack.scala - A widget for exploring an opetopic context
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx.opfibred

import scala.collection.mutable.ListBuffer

import scalafx.Includes._
import scalafx.geometry._
import scalafx.scene.paint._
import scalafx.scene.layout._
import scalafx.scene.control._

import opetopic._
import TypeLemmas._
import syntax.complex._

import scalaz.std.option._

import FibredLabel._

class ContextStack extends StackPane {

  type Context = FiniteComplex[FibredLabel]

  val universalContext : Context = 
    Complex[FibredLabel]() >> Obj(FibredLabel(Z)("U", Color.AliceBlue, Nil))

  val contextList : ListBuffer[Context] = 
    ListBuffer(universalContext)

  var editor : FibredEditor = createEditor(universalContext)

  var faceStack = createViewerStack
  var coloringStack = createViewerStack
  var editorStack = createViewerStack

  editorStack.children = editor

  def createViewerStack = 
    new StackPane {
      style = "-fx-border-style: solid; -fx-border-width: 1px; -fx-border-color: grey"
    }

  def createEditor(cntxt: Context) : FibredEditor = {

    val editor = new FibredEditor(cntxt)

    editor.onSelectAsRoot =
      new IndexedOp[editor.FXCardinalMarker] {
        def apply[N <: Nat](n: N)(mk: editor.FXCardinalMarker[N]) = {
          mk.label match {
            case Neutral(Some(fl)) => {
              for {
                fc <- mk.faceComplex
                lc <- fc traverse(new IndexedTraverse[Option, editor.MarkerType, FibredLabel] {
                  def apply[N <: Nat](n: N)(m: editor.MarkerType[N]) : Option[FibredLabel[N]] =
                    m.label match {
                      case Neutral(opt) => opt
                      case _ => None
                    }
                })
              } {

                lastFace = Some(lc)

                val faceViewer = new FibredViewer(lc)
                faceStack.children = faceViewer
                faceViewer.render

                for {
                  clr <- getContextFace(fl, cntxt)
                } { 

                  val coloringViewer = new FibredViewer(clr)
                  coloringStack.children = coloringViewer
                  coloringViewer.render

                }
              }
            }
            case _ => {
              faceStack.children.clear
              coloringStack.children.clear
            }
          }
        }
      }

    editor.render
    editor

  }

  def getContextFace[N <: Nat](fl: FibredLabel[N], cntxt: Context) : ShapeM[Context] =
    for {
      diff <- fromOpt(diffOpt(fl.coloringDim, cntxt.n))
      clr <- cntxt.value.sourceAt(fl.address)(diff)
    } yield clr

  var lastFace : Option[Context] = None

  def extendByContext : Unit = 
    lastFace map extendByContext

  def extendByContext(cntxt: Context) : Unit = {

    val prevContext = contextList.head

    for {
      cntxtFace <- getContextFace(cntxt.headValue, prevContext)
    } {

      val localFaceViewer = new FibredViewer(cntxt)
      val localColoringViewer = new FibredViewer(cntxtFace)

      val localFaceStack = createViewerStack
      val localColoringStack = createViewerStack

      HBox.setHgrow(localFaceStack, Priority.Always)
      HBox.setHgrow(localColoringStack, Priority.Always)

      localFaceStack.children = localFaceViewer
      localFaceViewer.render

      localColoringStack.children = localColoringViewer
      localColoringViewer.render

      // Setup a callback to render the labels
      localFaceViewer.onSelectAsRoot =
        new IndexedOp[localFaceViewer.MarkerType] {
          def apply[N <: Nat](n: N)(mk: localFaceViewer.MarkerType[N]) = {
            for {
              theCntxtFace <- getContextFace(mk.label, prevContext)
            } {
              val newViewer = new FibredViewer(theCntxtFace)
              localColoringStack.children = newViewer
              newViewer.render
            }
          }
        }

      val contextHBox = new HBox {
        children = List(localFaceStack, localColoringStack)
        spacing = 10
      }

      VBox.setVgrow(contextHBox, Priority.Always)
      contextHBox +=: vbox.children

      // Now reset all the grid elements

      editor = createEditor(cntxt)

      faceStack = createViewerStack
      coloringStack = createViewerStack
      editorStack = createViewerStack

      editorStack.children = editor

      setGridElements
      cntxt +=: contextList

    }

  }

  val baseViewerStack = new StackPane {
    children = { val viewer = new FibredViewer(universalContext) ; viewer.render ; viewer }
    style = "-fx-border-style: solid; -fx-border-width: 1px; -fx-border-color: grey"
    padding = Insets(10, 10, 10, 10)
  }

  VBox.setVgrow(baseViewerStack, Priority.Always)

  val vbox = new VBox {
    children = List(baseViewerStack)
    padding = Insets(10, 10, 10, 10)
    spacing = 10
  }

  val grid = new GridPane {
    rowConstraints = List(
      new RowConstraints { percentHeight = 70 },
      new RowConstraints { percentHeight = 30 }
    )
    columnConstraints = List(
      new ColumnConstraints { percentWidth = 50 },
      new ColumnConstraints { percentWidth = 50 }
    )
    hgap = 10
    vgap = 10
    padding = Insets(10, 10, 10, 10)
  }

  def setGridElements : Unit = {

    grid.children.clear

    GridPane.setColumnSpan(editorStack, 2)

    grid.add(editorStack, 0, 0)
    grid.add(faceStack, 0, 1)
    grid.add(coloringStack, 1, 1)

  }

  setGridElements

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(grid, vbox)
    dividerPositions = 0.5f
  }

  children = verticalSplit

}
