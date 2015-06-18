/**
  * ObjectPanel.scala - Specialized Panel for Object Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.newui

import opetopic._
import TypeDefs._

abstract class ObjectPanel[A, U : Numeric] extends Panel[A, U, _0] {

  import isNumeric._

  def layout : ShapeM[Unit] = {
    layout(nesting)
    succeed(())
  }

  def layout(nst : Nesting[MarkerType, _0]) : MarkerType = 
    nst match {
      case Obj(rm) => {
        rm.clear
        rm
      }
      case Box(rm, Pt(n)) => {

        val internalMarker = layout(n)

        rm.clear

        rm.leftInteriorMargin = internalMarker.leftMargin
        rm.rightInteriorMargin = internalMarker.rightMargin
        rm.interiorHeight = internalMarker.height

        internalMarker.shiftUp(fullStrokeWidth + rm.labelHeight)

        rm.horizontalDependants += internalMarker
        rm.verticalDependants += internalMarker

        rm
      }
    }

}
