/**
  * FXViewer.scala - An abstract base class for JavaFX viewers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx

import scala.language.higherKinds

import scala.collection.JavaConversions._

import javafx.scene.Node
import javafx.scene.Group
import javafx.scene.layout._
import javafx.scene.text.Text
import javafx.scene.paint.Color
import javafx.scene.input.MouseEvent
import javafx.scene.input.MouseButton
import javafx.scene.shape.{Box => _, _}
import javafx.scene.control.{ContextMenu, MenuItem, ColorPicker, TextField, Label}
import javafx.geometry.Insets
import javafx.geometry.Side
import javafx.event.EventHandler
import javafx.event.ActionEvent
import javafx.scene.control.Dialog
import javafx.scene.control.ButtonType
import javafx.util.Callback

import java.util.function.Consumer

import opetopic._
import opetopic.ui._
import TypeDefs._

abstract class FXViewer[A[_ <: Nat]](implicit ev : Numeric[Double]) extends Region with Viewer[A, Double] { thisViewer =>

  implicit val isNumeric = ev

  def arcRadius : Double = 4.0
  def halfLeafWidth : Double = 5.0
  def halfStrokeWidth : Double = 1.0
  def internalPadding : Double = 5.0
  def externalPadding : Double = 5.0

  type BoxType <: FXBox
  type EdgeType <: FXEdge

  type CanvasType <: FXCanvas
  type MarkerType[N <: Nat] <: FXMarker[N]

  override def render : Unit = {
    super.render
    setNeedsLayout(true)
  }

  def parseStringInput[N <: Nat](str: String) : A[N]

  //============================================================================================
  // RENDER MARKER IMPLEMENTATION
  //

  abstract class FXMarker[N <: Nat] extends ViewerMarker[N] { thisMarker : MarkerType[N] => 

    def halfLabelWidth = box.label.getLayoutBounds.getWidth / 2
    def halfLabelHeight = box.label.getLayoutBounds.getHeight / 2

    var outgoingEdgeMarker : Option[RenderMarker] = None

    var rootX : Double = 0.0
    var rootY : Double = 0.0

    override def toString = "Marker(" ++ label.toString ++ ")"

  }

  //============================================================================================
  // CANVAS IMPLEMENTATION
  //

  abstract class FXCanvas extends Region with ViewerCanvas { thisCanvas : CanvasType =>

    val group = new Group
    getChildren add group

    override def layoutChildren = {
      group.autosize
      group.relocate(getInsets.getLeft, getInsets.getTop)
    }

    def addBox(box : BoxType) : Unit =
      group.getChildren add box

    def addEdge(edge : EdgeType) : Unit =
      group.getChildren add edge

  }

  def displayCanvas(canvas : CanvasType) : Unit = 
    getChildren add canvas

  //============================================================================================
  // CONTEXT MENU
  //

  val propertiesItem = new MenuItem("Properties")
  val previewCellItem = new MenuItem("Preview Cell")

  val boxContextMenu = new ContextMenu
  boxContextMenu.getItems.addAll(propertiesItem, previewCellItem)

  //============================================================================================
  // PROPERTIES DIALOG
  //

  case class CellProperties(val label: String, val color: Color)

  class PropertiesDialog extends Dialog[CellProperties] {

    setTitle("Cell Properties")
    setHeaderText("Cell Properties")

    getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

    val cb = new Callback[ButtonType, CellProperties] { 
      def call(bt: ButtonType) : CellProperties = 
        bt match {
          case ButtonType.OK => CellProperties(labelField.getText, colorPicker.getValue)
          case ButtonType.CANCEL => CellProperties("", Color.WHITE)
        }
    }

    setResultConverter(cb)

    val grid = new GridPane
    grid.setHgap(10)
    grid.setVgap(10)

    val labelField = new TextField
    val colorPicker = new ColorPicker

    grid.add(new Label("Label: "), 0, 0)
    grid.add(labelField, 1, 0)
    grid.add(new Label("Color: "), 0, 1)
    grid.add(colorPicker, 1, 1)

    getDialogPane.setContent(grid)

  }

  def getCellProperties(box : FXBox) : Unit = {

    val dialog = new PropertiesDialog

    dialog.showAndWait.ifPresent(new Consumer[CellProperties] {
      def accept(cp: CellProperties) = {
        val newLabel = parseStringInput[box.Dim](cp.label)
        box.setBackground(box.genBg(cp.color))
        box.marker.label = newLabel
        render
      }
    })

  }

  //============================================================================================
  // BOXES
  //

  abstract class FXBox extends Region with ViewerBox { thisBox : BoxType => 

    def label : Node

    def genBg(color : Color) : Background = 
      new Background(
        new BackgroundFill(
          color,
          new CornerRadii(arcRadius),
          Insets.EMPTY
        )
      )

    val border = new Border(
      new BorderStroke(
        Color.BLACK, 
        BorderStrokeStyle.SOLID, 
        new CornerRadii(arcRadius), 
        new BorderWidths(strokeWidth)
      )
    )

    setBackground(genBg(Color.WHITE))
    setBorder(border)

    val pane = new Pane
    this.getChildren.add(pane)

    def doHoverStyle : Unit = ()
    def doUnhoverStyle : Unit = ()
    def doSelectedStyle : Unit = ()
    def doUnselectedStyle : Unit = ()

    def render : Unit = {
      relocate(marker.x, marker.y)
      setPrefWidth(marker.width)
      setPrefHeight(marker.height)
    }

    val mouseHandler =
      new EventHandler[MouseEvent] {
        def handle(ev : MouseEvent) {
          ev.getEventType match {
            case MouseEvent.MOUSE_ENTERED => marker.hover
            case MouseEvent.MOUSE_EXITED => marker.unhover
            case MouseEvent.MOUSE_CLICKED => 
              ev.getButton match {
                case MouseButton.PRIMARY => 
                  if (ev.isControlDown())
                    select(marker)
                  else
                    selectAsRoot(marker)
                case MouseButton.SECONDARY => {

                  propertiesItem.setOnAction(new EventHandler[ActionEvent] {
                    def handle(ev: ActionEvent) = getCellProperties(thisBox)
                  })

                  boxContextMenu.show(thisBox, Side.TOP, 
                    labelXPos + marker.halfLabelWidth, 
                    labelYPos + marker.halfLabelHeight
                  )
                }
                case _ => ()
              }
            case _ => ()
          }
          ev.consume
        }
      }

    pane.addEventHandler(MouseEvent.ANY, mouseHandler)

    def labelXPos : Double = getWidth - strokeWidth - internalPadding - marker.labelWidth 
    def labelYPos : Double = getHeight - strokeWidth - internalPadding - marker.labelHeight

    override def layoutChildren = {
      pane.resizeRelocate(0, 0, getWidth, getHeight)
      label.relocate(labelXPos, labelYPos)
    }

  }

  //============================================================================================
  // EDGES
  //

  abstract class FXEdge extends Path with ViewerEdge { thisEdge : EdgeType => 

    setStroke(Color.BLACK)
    setStrokeWidth(strokeWidth)

    setMouseTransparent(true)

    def render = {
      val startMove = new MoveTo(marker.edgeStartX, marker.edgeStartY)

      if (marker.edgeStartX == marker.edgeEndX) {
        val vertLine = new VLineTo(marker.edgeEndY)
        getElements.setAll(List(startMove, vertLine))
      } else {
        val vertLine = new VLineTo(marker.edgeEndY - arcRadius)

        val arcTo = new ArcTo
        arcTo.setX(if (marker.edgeStartX > marker.edgeEndX) (marker.edgeStartX - arcRadius) else (marker.edgeStartX + arcRadius))
        arcTo.setY(marker.edgeEndY)
        arcTo.setRadiusX(arcRadius)
        arcTo.setRadiusY(arcRadius)
        arcTo.setSweepFlag(marker.edgeStartX > marker.edgeEndX)

        val horizLine = new HLineTo(marker.edgeEndX)

        getElements.setAll(List(startMove, vertLine, arcTo, horizLine))
      }
    }

    def doHoverStyle : Unit = ()
    def doUnhoverStyle : Unit = ()
    def doSelectedStyle : Unit = ()
    def doUnselectedStyle : Unit = ()

  }

  //============================================================================================
  // LAYOUT ROUTINES
  //

  var spacing : Double = 10.0
  var maxHeight : Double = 0.0

  override def computePrefWidth(height : Double) : Double = {
    val pWidth : Double = 
      (getChildren foldLeft 0.0)({
        case (totalWidth, child) => totalWidth + child.prefWidth(height) + spacing
      })

    if (pWidth > 0.0) pWidth - spacing else pWidth
  }

  override def computePrefHeight(width : Double) : Double = {
    maxHeight = (getChildren foldLeft 0.0)({
      case (maxHeight, child) => Math.max(child.prefHeight(width), maxHeight)
    })
    maxHeight
  }

  override def layoutChildren = {
    (getChildren foldLeft 0.0)({
      case (leftShift, child) => {
        val childWidth = child.prefWidth(-1)
        val childHeight = child.prefHeight(-1)
        val heightOffset = (maxHeight - childHeight) / 2
        child.resizeRelocate(leftShift, heightOffset, childWidth, childHeight)
        leftShift + childWidth + spacing
      }
    })
  }

}
