/**
  * FibredEditor.scala - A cardinal editor for OpFibred
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx.opfibred

import javafx.scene.paint.Color
import javafx.scene.control._
import javafx.scene.layout._
import javafx.geometry._
import javafx.event._

import javafx.util.Callback
import java.util.function.Consumer

import scalaz.std.option._

import opetopic._
import opetopic.fx._
import TypeDefs._
import syntax.complex._
import syntax.cardinal._

import FibredLabel._

class FibredEditor(c: FiniteCardinal[FibredLblOpt], val context: FiniteComplex[FibredLabel]) 
    extends FXCardinalEditor[FibredLabel](c) { thisEditor =>

  def this(cntxt: FiniteComplex[FibredLabel]) = 
    this(Cardinal[FibredLblOpt]() >> Pt(Obj(None)), cntxt)

  type NeutralBoxType[N <: Nat] = FibredCellBox[N]

  def createNeutralBox[N <: Nat](mk: FXNeutralMarker[N]) : FibredCellBox[N] = 
    new FibredCellBox(mk)

  class FibredCellBox[N <: Nat](mk: FXNeutralMarker[N]) extends FXNeutralBox[N] { thisBox =>

    def marker: FXNeutralMarker[N] = mk

    def color : Color = 
      marker.element match {
        case None => Color.WHITE
        case Some(fl) => fl.color
      }

    override def onMouseRightClick : Unit = {

      // editItem.setOnAction(new EventHandler[ActionEvent] {
      //   def handle(ev: ActionEvent) = editCellContent
      // })

      // asContextItem.setOnAction(new EventHandler[ActionEvent] {
      //   def handle(ev: ActionEvent) = {
      //     for {
      //       fc <- marker.faceComplex
      //       lblCmplx <- fc.traverse(new IndexedTraverse[Option, MarkerType, FibredLabel] {
      //         def apply[N <: Nat](n: N)(mk: MarkerType[N]) : Option[FibredLabel[N]] = 
      //           mk.label match {
      //             case Neutral(opt) => opt
      //             case _ => None
      //           }
      //       })
      //     } { builder.exportContext(lblCmplx) }
      //   }
      // })

      cellContextMenu.show(thisBox, Side.TOP,
        labelXPos + marker.halfLabelWidth,
        labelYPos + marker.halfLabelHeight
      )

    }

    override def onMouseDoubleClick : Unit = editCellContent

    def editCellContent : Unit = {

      val cellDialog = new CellEditorDialog[N]

      marker.element match {
        case None => cellDialog.colorPicker.setValue(lastColor)
        case Some(fl) => {
          cellDialog.colorPicker.setValue(fl.color)
          cellDialog.labelField.setText(fl.label)
          // Now you should select the element in the viewer which corresponds 
          // to the label on this face ....
        }
      }

      javafx.application.Platform.runLater(new Runnable {
        def run : Unit = cellDialog.labelField.requestFocus
      })

      cellDialog.showAndWait.ifPresent(new Consumer[FibredLblOpt[N]] {
        def accept(lblOpt: FibredLblOpt[N]) = {
          marker.element = lblOpt
          for { fl <- lblOpt } { lastColor = fl.color }
          thisEditor.render   // Er, well, don't render everything ....
        }
      })

    }


  }

  //============================================================================================
  // CELL CONTEXT MENU
  //

  val editItem = new MenuItem("Edit")
  val asContextItem = new MenuItem("Use As Context")

  val cellContextMenu = new ContextMenu
  cellContextMenu.getItems.addAll(editItem, asContextItem)

  //============================================================================================
  // CELL EDITOR DIALOG
  //

  var lastColor : Color = Color.WHITE

  class CellEditorDialog[N <: Nat] extends Dialog[FibredLblOpt[N]] {

    setTitle("Edit Cell")
    setHeaderText("Cell Content")

    getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val cb = new Callback[ButtonType, FibredLblOpt[N]] { 
      def call(bt: ButtonType) : FibredLblOpt[N] = 
        bt match {
          case ButtonType.OK => {
            for {
              sel <- contextViewer.selection
            } yield {
              FibredLabel(sel.dim)(labelField.getText, colorPicker.getValue, sel.root.address)
            }
          }
          case ButtonType.CANCEL => None
        }
    }

    setResultConverter(cb)

    val grid = new GridPane
    grid.setHgap(10)
    grid.setVgap(10)

    val labelField = new TextField
    val colorPicker = new ColorPicker

    val contextViewer = new FibredViewer(context)

    val viewerStack = new StackPane 
    viewerStack.getChildren add contextViewer
    viewerStack.setStyle("-fx-border-style: solid; -fx-border-width: 1px; -fx-border-color: grey")
    viewerStack.setMinSize(200, 100)

    GridPane.setColumnSpan(viewerStack, 2)

    grid.add(new Label("Label: "), 0, 0)
    grid.add(labelField, 1, 0)
    grid.add(new Label("Color: "), 0, 1)
    grid.add(colorPicker, 1, 1)
    grid.add(viewerStack, 0, 2)

    getDialogPane.setContent(grid)

    contextViewer.render

  }

}
 
