/**
  * FXViewer.scala - An abstract base class for JavaFX viewers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx

import scala.language.higherKinds
import scala.collection.mutable.ListBuffer
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
import javafx.scene.transform.Scale
import javafx.event.EventHandler
import javafx.event.ActionEvent
import javafx.scene.control.Dialog
import javafx.scene.control.ButtonType
import javafx.util.Callback

import java.util.function.Consumer

import opetopic._
import opetopic.oldui._

abstract class FXViewer[A[_ <: Nat]](implicit ev : Numeric[Double]) extends Region with Viewer[A, Double] { thisViewer =>

  implicit val isNumeric = ev

  def arcRadius : Double = 4.0
  def halfLeafWidth : Double = 8.0
  def halfStrokeWidth : Double = 1.0
  def internalPadding : Double = 5.0
  def externalPadding : Double = 5.0

  type BoxType <: FXBox
  type EdgeType <: FXEdge

  type CanvasType <: FXCanvas
  type MarkerType[N <: Nat] <: FXMarker[N]

  val canvases : ListBuffer[CanvasType] = new ListBuffer()

  override def render : ShapeM[Unit] = {
    for {
      _ <- super.render
    } yield {
      for {
        canvas <- canvases
      } {
        canvas.requestLayout()
      }
    }
  }

  // setStyle("-fx-border-style: solid; -fx-border-size: 2pt; -fx-border-color: purple")
  setMaxSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)

  //============================================================================================
  // RENDER MARKER IMPLEMENTATION
  //

  abstract class FXMarker[N <: Nat] extends ViewerMarker[N] { thisMarker : MarkerType[N] => 

    def halfLabelWidth = box.label.getLayoutBounds.getWidth / 2
    def halfLabelHeight = box.label.getLayoutBounds.getHeight / 2

    var outgoingEdgeMarker : Option[RenderMarker] = None

    var rootX : Double = 0.0
    var rootY : Double = 0.0

    override def toString = "FXMarker(" ++ label.toString ++ ")"

  }

  //============================================================================================
  // CANVAS IMPLEMENTATION
  //

  abstract class FXCanvas extends Region with ViewerCanvas { thisCanvas : CanvasType =>

    protected val group = new Group
    protected val groupScale = new Scale(1.0, 1.0, 0.0, 0.0)

    group.setManaged(false)
    group.getTransforms() add groupScale
    getChildren add group

    // setStyle("-fx-border-style: solid; -fx-border-size: 2pt; -fx-border-color: green")

    def addBox(box : BoxType) : Unit = 
      group.getChildren add box

    def addEdge(edge : EdgeType) : Unit = 
      group.getChildren add edge

    private var zoomFactor : Double = 1.0

    def setZoomFactor(zf : Double) : Unit = {
      zoomFactor = zf
      requestLayout
    }

    override def layoutChildren = {
      val bounds = group.getBoundsInParent()
      val emptyX = getWidth() - getInsets.getLeft - getInsets.getRight - bounds.getWidth
      val emptyY = getHeight() - getInsets.getTop - getInsets.getBottom - bounds.getHeight
      group.relocate(getInsets.getLeft + (emptyX / 2), getInsets.getTop + (emptyY / 2))
    }

    override def computePrefWidth(height : Double) : Double = {
      getInsets.getLeft + group.prefWidth(height) + getInsets.getRight
    }

    override def computePrefHeight(width : Double) : Double = {
      getInsets.getTop + group.prefHeight(width) + getInsets.getBottom
    }

    override def resize(width : Double, height : Double) = {

      val bounds = group.getLayoutBounds

      groupScale.setPivotX(bounds.getMinX)
      groupScale.setPivotY(bounds.getMinY)

      val xfactor = (width - getInsets.getLeft - getInsets.getRight) / bounds.getWidth
      val yfactor = (height - getInsets.getTop - getInsets.getBottom) / bounds.getHeight

      if (xfactor < yfactor) {
        if (xfactor <= 1.0) {
          groupScale.setX(xfactor * zoomFactor)
          groupScale.setY(xfactor * zoomFactor)
        } else {
          groupScale.setX(zoomFactor)
          groupScale.setY(zoomFactor)
        }
      } else {
        if (yfactor <= 1.0) {
          groupScale.setX(yfactor * zoomFactor)
          groupScale.setY(yfactor * zoomFactor)
        } else {
          groupScale.setX(zoomFactor)
          groupScale.setY(zoomFactor)
        }
      }

      super.resize(width, height)

    }
  }

  def displayCanvas(canvas : CanvasType) : Unit = 
    getChildren add canvas

  //============================================================================================
  // CONTEXT MENU
  //

  // val propertiesItem = new MenuItem("Properties")
  // val previewCellItem = new MenuItem("Preview Cell")

  // val boxContextMenu = new ContextMenu
  // boxContextMenu.getItems.addAll(propertiesItem, previewCellItem)

  //============================================================================================
  // BOXES
  //

  abstract class FXBox extends Region with ViewerBox { thisBox : BoxType => 

    def label : Node
    def color : Color

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

    setBorder(border)

    val pane = new Pane
    this.getChildren.add(pane)

    def doHoverStyle : Unit = ()
    def doUnhoverStyle : Unit = ()
    def doSelectedStyle : Unit = ()
    def doUnselectedStyle : Unit = ()

    def render : Unit = {
      setBackground(genBg(color))
      relocate(marker.x, marker.y)
      setPrefWidth(marker.width)
      setPrefHeight(marker.height)
    }

    val mouseHandler =
      new EventHandler[MouseEvent] {
        def handle(ev : MouseEvent) {
          ev.getEventType match {
            case MouseEvent.MOUSE_ENTERED => onMouseEnter
            case MouseEvent.MOUSE_EXITED => onMouseExit
            case MouseEvent.MOUSE_CLICKED => {
              ev.getButton match {
                case MouseButton.PRIMARY => 
                  if (ev.isControlDown())
                    onMouseCtrlClick
                  else if (ev.getClickCount > 1) 
                    onMouseDoubleClick
                  else
                    onMouseClick
                case MouseButton.SECONDARY => onMouseRightClick
                case _ => ()
              }
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

      toFront
    }

    def doHoverStyle : Unit = ()
    def doUnhoverStyle : Unit = ()
    def doSelectedStyle : Unit = ()
    def doUnselectedStyle : Unit = ()

  }

  //============================================================================================
  // EVENT HANDLING
  //

  addEventHandler(MouseEvent.ANY,
    new EventHandler[MouseEvent] {
      def handle(ev : MouseEvent) {
        ev.getEventType match {
          case MouseEvent.MOUSE_CLICKED => 
            deselectAll
          case _ => ()
        }
        ev.consume
      }
    }
  )

  //============================================================================================
  // LAYOUT ROUTINES
  //

  var spacing : Double = 25.0
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
        val childWidth = child.prefWidth(0)
        val childHeight = child.prefHeight(0)

        val (heightOffset, childHSize) = 
          if (childHeight > getHeight) {
            (0.0, getHeight)
          } else {
            ((getHeight - childHeight) / 2, childHeight)
          }

        child.resizeRelocate(leftShift, heightOffset, childWidth, childHSize)
        leftShift + childWidth + spacing
      }
    })
  }

}
