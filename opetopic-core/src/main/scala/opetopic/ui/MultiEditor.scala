/**
  * MultiEditor.scala - An editor for Multitopes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

class MultiEditor[A: Renderable, F <: ActiveFramework](frmwk: F) {

  type OptA = Option[A]

  val levelEditor: StableEditor[A, F] = new StableEditor(frmwk)(SCardinal(||(SDot(None))))
  val valueEditor: StableEditor[A, F] = new StableEditor(frmwk)(SCardinal(||(SDot(None))))

  // Start with a trivial multitope
  var multitope: Multitope[Option[A]] = Base(||(SDot(None)))
  var levels: Seq[SComplex[Option[A]]] = Seq(||(SDot(None)))

  def newLevel: Unit = {
    // Also add a new level model ...
    multitope = Up(||(SDot(multitope)))
  }

  def openLevel(i: Int): Unit = {
    val editor = new StableEditor(frmwk)(SCardinal(levels(i)))
  }

  // Right.  How are you going to make contact between the the extrusion functionality
  // of the editor and the multitope that is being edited?

  // So, the idea of the next definition is that you are in one of two editing modes:
  // either the values, or else the levels, in which case the extrusion function will
  // if fact produce different effects.

  var editor: Either[StableEditor[A, F], StableEditor[Multitope[Option[A]], F]] = ???

  // Okay, but just the editor is not really enough.  You should have a kind of
  // "context" class which saves the the path travelled to the current level as well.
  // Possibly here you can save a model of what exactly should be tagged onto the
  // new extended elements.

}

