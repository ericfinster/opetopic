/**
  * Render.scala - Typeclass based rendering
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.newui

import opetopic._
import TypeDefs._

trait Rooted[U, R] { 

  def rootX(r: R) : U
  def rootY(r: R) : U

  def addHorizontalDependent[D](r: R, dep: D)(implicit isRooted: Rooted[U, D]) : Unit
  def addVerticalDependent[D](r: R, dep: D)(implicit isRooted: Rooted[U, D]) : Unit

  def clearHorizontalDependents : Unit
  def clearVerticalDependents : Unit

  def shiftLeft(r: R, amount: U) : Unit
  def shiftRight(r: R, amount: U) : Unit
  def shiftUp(r: R, amount: U) : Unit
  def shiftDown(r: R, amount: U) : Unit

}

object Rooted {

  class RootedOps[U, R](r: R)(implicit isRooted: Rooted[U, R]) {

    def rootX : U = isRooted.rootX(r)
    def rootY : U = isRooted.rootY(r)

  }

  implicit def toRootedOps[U, R](r: R)(implicit isRooted: Rooted[U, R]) : RootedOps[U, R] =
    new RootedOps[U, R](r)

}

// Should these *all* be properties with getters and setters?

trait RenderMarker[U, M] extends Rooted[U, M] {

  def isExternal(mk: M) : Boolean

  def edgeMarker(mk: M) : M  // Possibly should be an option ...

  def halfLabelWidth(mk: M) : U
  def halfLabelHeight(mk: M) : U

  def getLeftInteriorMargin(mk: M) : U
  def setLeftInteriorMargin(mk: M, u: U) : Unit

  def getRightInteriorMargin(mk: M) : U
  def setRightInteriorMargin(mk: M, u: U) : Unit

  def getInteriorHeight(mk: M) : U
  def setInteriorHeight(mk: M, u: U) : Unit

  def clear(mk: M) : Unit

}

object RenderMarker {

  class MarkerOps[U, M](mk: M)(
    implicit isNumeric: Numeric[U], isMarker: RenderMarker[U, M]
  ) {

    import isNumeric._
    import isMarker._

    def x : U = rootX(mk) - leftMargin
    def y : U = rootY(mk) - height

    def leftMargin : U = ???
      // if (isExternal(mk)) {
      //   halfLabelWidth + internalPadding + strokeWidth
      // } else {
      //   strokeWidth + leftInteriorMargin + internalPadding + strokeWidth
      // }

    def rightMargin : U = ???
      // if (isExternal) {
      //   halfLabelWidth + internalPadding + strokeWidth
      // } else {
      //   max(
      //     labelContainerWidth + strokeWidth,
      //     strokeWidth + rightInteriorMargin + internalPadding + strokeWidth
      //   )
      // }

    def width : U = ??? // leftMargin + rightMargin 
    def height : U = ???
      // if (isExternal) {
      //   strokeWidth + 
      //   internalPadding + 
      //   labelHeight + 
      //   internalPadding + 
      //   strokeWidth
      // } else {
      //   strokeWidth +
      //   labelContainerHeight +
      //   interiorHeight +
      //   internalPadding +
      //   strokeWidth
      // }

    def labelWidth : U = fromInt(2) * halfLabelWidth(mk)
    def labelHeight : U = fromInt(2) * halfLabelHeight(mk)

    def leftInteriorMargin: U = getLeftInteriorMargin(mk)
    def leftInteriorMargin_=(u: U) : Unit = setLeftInteriorMargin(mk, u)

    def rightInteriorMargin: U = getRightInteriorMargin(mk)
    def rightInteriorMargin_=(u: U) : Unit = setRightInteriorMargin(mk, u)

    def interiorHeight: U = getInteriorHeight(mk)
    def interiorHeight_=(u: U) : Unit = setInteriorHeight(mk, u)

    def clear: Unit = isMarker.clear(mk)

  }

  implicit def toMarkerOps[U, M](mk: M)(
    implicit isNumeric: Numeric[U], isMarker: RenderMarker[U, M]
  ) : MarkerOps[U, M] =
    new MarkerOps[U, M](mk)

}

//============================================================================================
// LAYOUT MARKER
//

abstract class LayoutMarker[U, R, M](
  implicit isNumeric: Numeric[U], isRooted: Rooted[U, R], isMarker: RenderMarker[U, M]
) { thisMarker =>

  import isNumeric._
  import Rooted._

  val element : R
  val edgeMarker : M

  val isLocal: Boolean

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

  def truncateLeft : LayoutMarker[U, R, M] =
    new LayoutMarker[U, R, M] {
      val element = thisMarker.element
      val edgeMarker = thisMarker.edgeMarker
      val isLocal = true
      override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
      override def rightInternalMargin = thisMarker.rightInternalMargin
    }

  def truncateRight : LayoutMarker[U, R, M] =
    new LayoutMarker[U, R, M] {
      val element = thisMarker.element
      val edgeMarker = thisMarker.edgeMarker
      val isLocal = true
      override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
      override def leftInternalMargin = thisMarker.leftInternalMargin
    }

  def truncateUnique : LayoutMarker[U, R, M] =
    new LayoutMarker[U, R, M] {
      val element = thisMarker.element
      val edgeMarker = thisMarker.edgeMarker
      val isLocal = true
    }

  def truncateMiddle : LayoutMarker[U, R, M] =
    new LayoutMarker[U, R, M] {
      val element = thisMarker.element
      val edgeMarker = thisMarker.edgeMarker
      val isLocal = true
      override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
      override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
      override def leftInternalMargin = thisMarker.leftInternalMargin
      override def rightInternalMargin = thisMarker.rightInternalMargin
    }

  override def toString = "LM(" ++ element.toString ++ ")" ++
  "(il = " ++ isLocal.toString ++ ", ht = " ++ height.toString ++
  ", em = " ++ edgeMarker.toString ++
  ", rx = " ++ element.rootX.toString ++
  ", ry = " ++ element.rootY.toString ++
  ", lsm = " ++ leftSubtreeMargin.toString ++
  ", lim = " ++ leftInternalMargin.toString ++
  ", rim = " ++ rightInternalMargin.toString ++
  ", rsm = " ++ rightSubtreeMargin.toString ++ ")"

}

