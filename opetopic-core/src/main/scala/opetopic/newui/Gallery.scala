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

  type PanelSuite[N <: Nat] = Suite[Lambda[`K <: Nat` => Panel[A[K], U, K]], N]

  def panels : Sigma[PanelSuite]

  // Put routines to render individual panels and ranges of the
  // panels ....

  // Hmmm.  Maybe the idea is just fuck it.  We take the kind of react
  // approach and assume in the code that we re-render everything every time.
  // The idea will be that in a backend, we can do some kind of speedup

  // So, now what about event handling and state?

  // The thing is that when you render to a static svg file using a scalatags
  // backend, none of the scala code will be available.  Do I care?

  // I think what I would like to go ahead and do is start pulling out the react
  // stuff and at the same time, write one of these scalatag agnostic backends
  // which then get instantiated to the various versions for in-browser and static
  // generation.

}
