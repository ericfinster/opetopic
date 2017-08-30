/**
  * Mtt.scala - Makkai Style Type Theory Main Application
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.mtt

import scala.scalajs.js.JSApp

object Mtt extends JSApp with MttUI {

  def main: Unit = {
    println("Starting Mtt ...")
    initializeUI
  }

}

// Great, so we have a simple editor up and going and we can start
// to customize things.

// What's frustrating, I guess, is that you know exactly how
// the position of an identity should be specified: it is exactly
// an arrow in the link about a given object.

// Yeah, so it would seem like you have to finish the link construction
// to really make this work nicely.
