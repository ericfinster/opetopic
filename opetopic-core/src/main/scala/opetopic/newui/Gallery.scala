/**
  * Gallery.scala - An abstract gallery collecting together the panels of a complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.newui

import opetopic._
import TypeDefs._

abstract class Gallery[A[_ <: Nat], U : Numeric] {

  type PanelType[K <: Nat] <: Panel[A[K], U, K]
  type PanelSuite[N <: Nat] = Suite[PanelType, N]

  def panels : Sigma[PanelSuite]

}
