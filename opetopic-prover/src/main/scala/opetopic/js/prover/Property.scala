/**
  * Property.scala - Encapsulate properties of cell expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import opetopic._
import ott.OttSyntax._

sealed trait Property {
  def propertyId: String
  def propertyExp: ExpT
  def propertyType: ExpT
  def cellId: String
  def cellExp: ExpT
  def isTarget: Boolean
  def isSourceAt(a: SAddr): Boolean
}

case class TgtExtProperty(
  val propertyId: String,
  val propertyExp: ExpT,
  val propertyType: ExpT,
  val cellId: String,
  val cellExp: ExpT
) extends Property {
  def isTarget: Boolean = true
  def isSourceAt(a: SAddr): Boolean = false
}

case class SrcExtProperty(
  val propertyId: String,
  val propertyExp: ExpT,
  val propertyType: ExpT,
  val addr: SAddr,
  val cellId: String,
  val cellExp: ExpT
) extends Property {
  def isTarget: Boolean = false
  def isSourceAt(a: SAddr): Boolean = a == addr
}
