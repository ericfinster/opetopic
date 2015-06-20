/**
  * Panel.scala - A Panel which can layout a nesting
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.newui

import opetopic._
import TypeDefs._

import syntax.tree._
import syntax.nesting._

abstract class Panel[A, U : Numeric, N <: Nat] extends RenderingContext[U] {

  type MarkerType <: PanelMarker

  abstract class PanelMarker extends RenderMarker {

    def label: A
    def address: Address[S[N]]


  }

  def createMarker(lbl: A, addr: Address[S[N]], isExt: Boolean) : MarkerType

  def generateMarkers(n: N)(nst: Nesting[A, N]) : Nesting[MarkerType, N] =
    Nesting.elimWithAddress[A, Nesting[MarkerType, N], N](n)(nst)({
      case (a, addr) => Nesting.external(n)(createMarker(a, addr, true))
    })({
      case (a, addr, cn) => Box(createMarker(a, addr, false), cn)
    })

  def nesting: Nesting[MarkerType, N]

  def layout : ShapeM[Unit]

  // Yeah, this is no good.  What we should be producing here is like vitual-dom:
  // the canvas is an intermidiary which abstracts over the return type of render
  // (which should be a type variable instantiated for the various implementations)
  // For scalafx, it will be Node.  For scalatags, the various frontend/backend
  // capabilities.

  def render(canvas: Canvas[U]) : Unit = 
    for {
      mk <- nesting
    } {

      import isNumeric._

      canvas.rect(mk.x, mk.y, mk.width, mk.height, fromInt(4)) // Got to add the arc radius setting ...

      val labelXPos = mk.x + mk.width - fullStrokeWidth - internalPadding - mk.labelWidth
      val labelYPos = mk.y + mk.height - fullStrokeWidth - internalPadding
      
      canvas.text(labelXPos, labelYPos, mk.label.toString)

    }

}


