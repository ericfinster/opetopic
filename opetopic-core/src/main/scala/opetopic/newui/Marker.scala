/**
  * Marker.scala - A Marker Type Class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.newui

import opetopic._

trait Marker[A[_ <: Nat], U] {

  def isExternal[N <: Nat](a: A[N]) : Boolean

  def rootX[N <: Nat](a: A[N]) : U
  def rootY[N <: Nat](a: A[N]) : U

  // def addHorizontalDependent


}
