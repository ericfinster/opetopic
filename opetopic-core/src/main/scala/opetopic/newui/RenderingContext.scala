/**
  * RenderingContext.scala - A Context for Rendering Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.newui

import scala.collection.mutable.ListBuffer

import opetopic._
import TypeDefs._

abstract class RenderingContext[U : Numeric] {

  val isNumeric = implicitly[Numeric[U]]
  import isNumeric._

  //============================================================================================
  // RENDERING OPTIONS
  //

  def internalPadding : U
  def externalPadding : U

  def halfLeafWidth : U
  def halfStrokeWidth : U

  def fullStrokeWidth = fromInt(2) * halfStrokeWidth
  def leafWidth = fromInt(2) * halfLeafWidth

  //============================================================================================
  // ROOTED ELEMENTS
  //

  trait RootedElement {

    var rootX : U 
    var rootY : U 

    val horizontalDependants : ListBuffer[RootedElement] = ListBuffer.empty
    val verticalDependants : ListBuffer[RootedElement] = ListBuffer.empty

    def shiftRight(amount : U) : Unit = {
      if (amount != 0) {
        rootX = (rootX + amount)
        horizontalDependants foreach (_.shiftRight(amount))
      }
    }

    def shiftDown(amount : U) : Unit = {
      if (amount != 0) {
        rootY = (rootY + amount)
        verticalDependants foreach (_.shiftDown(amount))
      }
    }

    def shiftLeft(amount : U) : Unit = shiftRight(-amount)
    def shiftUp(amount : U) : Unit = shiftDown(-amount)

  }

  //============================================================================================
  // EDGE MARKER
  //

  class EdgeMarker {

    // Edge Data

    var edgeStartX : U = zero
    var edgeStartY : U = zero

    var edgeEndX : U = zero
    var edgeEndY : U = zero

  }

  //============================================================================================
  // RENDER MARKER 
  //

  trait RenderMarker extends RootedElement {

    //
    // Mutable Values
    //

    var isExternal : Boolean = false

    var leftInteriorMargin : U = zero
    var rightInteriorMargin : U = zero

    var interiorHeight : U = zero

    // This I don't like ...
    var outgoingEdgeMarker : Option[EdgeMarker] = None

    //
    // Derived Values
    //

    def x : U = rootX - leftMargin
    def y : U = rootY - height

    def interiorWidth : U = leftInteriorMargin + rightInteriorMargin 

    def width : U = leftMargin + rightMargin 
    def height : U =
      if (isExternal) {
        fullStrokeWidth + 
        internalPadding + 
        labelHeight + 
        internalPadding + 
        fullStrokeWidth
      } else {
        fullStrokeWidth +
        interiorHeight +
        internalPadding +
        labelHeight +
        internalPadding + 
        fullStrokeWidth
      }

    def leftMargin : U =
      if (isExternal) {
        fullStrokeWidth + internalPadding + halfLabelWidth
      } else {
        fullStrokeWidth + leftInteriorMargin + internalPadding + fullStrokeWidth
      }

    def rightMargin : U =
      if (isExternal) {
        halfLabelWidth + internalPadding + fullStrokeWidth
      } else {
        max(
          internalPadding + labelWidth + internalPadding + fullStrokeWidth,
          rightInteriorMargin + internalPadding + fullStrokeWidth
        )
      }

    def halfLabelWidth : U
    def halfLabelHeight : U

    def labelWidth : U = halfLabelWidth * fromInt(2)
    def labelHeight : U = halfLabelHeight * fromInt(2)

    def clear : Unit = {
      rootX = zero
      rootY = zero
      leftInteriorMargin = zero
      rightInteriorMargin = zero 
      interiorHeight = zero
      horizontalDependants.clear
      verticalDependants.clear
    }

  }

  //============================================================================================
  // LAYOUT MARKER
  //

  abstract class LayoutMarker { thisMarker =>

    val element : RootedElement
    val external : Boolean  // This is really a bad name for this .. you should change it 

    val rootEdgeMarker : EdgeMarker

    def height : U = zero

    def leftSubtreeMargin : U = zero
    def rightSubtreeMargin : U = zero
    def leftInternalMargin : U = zero
    def rightInternalMargin : U = zero

    def leftMargin : U = leftSubtreeMargin + leftInternalMargin 
    def rightMargin : U = rightSubtreeMargin + rightInternalMargin

    def leftEdge : U = element.rootX - leftMargin
    def rightEdge : U = element.rootX + rightMargin

    // Truncations

    def truncateLeft : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val external = true
        val rootEdgeMarker = thisMarker.rootEdgeMarker
        override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
        override def rightInternalMargin = thisMarker.rightInternalMargin
      }

    def truncateRight : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val external = true
        val rootEdgeMarker = thisMarker.rootEdgeMarker
        override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
        override def leftInternalMargin = thisMarker.leftInternalMargin
      }

    def truncateUnique : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val rootEdgeMarker = thisMarker.rootEdgeMarker
        val external = true
      }

    def truncateMiddle : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val external = true
        val rootEdgeMarker = thisMarker.rootEdgeMarker
        override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
        override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
        override def leftInternalMargin = thisMarker.leftInternalMargin
        override def rightInternalMargin = thisMarker.rightInternalMargin
      }

    override def toString = "LM(" ++ element.toString ++ ")" ++ 
      "(we = " ++ external.toString ++ ", ht = " ++ height.toString ++
      ", re = " ++ rootEdgeMarker.toString ++
      ", rx = " ++ element.rootX.toString ++
      ", ry = " ++ element.rootY.toString ++
      ", lsm = " ++ leftSubtreeMargin.toString ++
      ", lim = " ++ leftInternalMargin.toString ++
      ", rim = " ++ rightInternalMargin.toString ++
      ", rsm = " ++ rightSubtreeMargin.toString ++ ")"

  }

}
