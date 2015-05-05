/**
  * FibredDialogs.scala - Custom Dialogs for OpFibred
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx.opfibred

import javafx.util.Callback

import javafx.scene.paint.Color
import javafx.scene.layout.GridPane
import javafx.scene.control.Label
import javafx.scene.control.TextField
import javafx.scene.control.Dialog
import javafx.scene.control.DialogEvent
import javafx.scene.control.ButtonType
import javafx.scene.control.TextArea
import javafx.scene.control.ColorPicker

import javafx.event.EventHandler

import opetopic._

object FibredDialogs {


  // class CellEditorDialog[N <: Nat] extends Dialog[Option[ColoredLabel[N]]] {

  //   setTitle("Cell Properties")
  //   setHeaderText("Cell Properties")

  //   getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

  //   val cb = new Callback[ButtonType, Option[ColoredLabel[N]]] { 
  //     def call(bt: ButtonType) : Option[ColoredLabel[N]] = 
  //       bt match {
  //         case ButtonType.OK => Some(ColoredLabel(labelField.getText, colorPicker.getValue))
  //         case ButtonType.CANCEL => None
  //       }
  //   }

  //   // setOnShown(new EventHandler[DialogEvent] {
  //   //   def handle(ev : DialogEvent) = {
  //   //     grid.requestFocus
  //   //     labelField.requestFocus
  //   //   }
  //   // })

  //   setResultConverter(cb)

  //   val grid = new GridPane
  //   grid.setHgap(10)
  //   grid.setVgap(10)

  //   val labelField = new TextField
  //   val colorPicker = new ColorPicker

  //   grid.add(new Label("Label: "), 0, 0)
  //   grid.add(labelField, 1, 0)
  //   grid.add(new Label("Color: "), 0, 1)
  //   grid.add(colorPicker, 1, 1)

  //   getDialogPane.setContent(grid)

  // }


}
