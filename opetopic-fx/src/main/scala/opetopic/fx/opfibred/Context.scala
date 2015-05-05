/**
  * Context.scala - Context Objects
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx.opfibred

import javafx.scene.paint.Color

import opetopic._
import TypeDefs._
import syntax.complex._

class Context(val name: String, val complex: FiniteComplex[FibredLabel]) {

  override def toString = name

}

object Context {

  object Empty extends Context(
    "Empty",
    Complex[FibredLabel]() >> Obj(
      new FibredLabel(
        "U",
        Color.ALICEBLUE,
        Sigma[Address, _0](Z)(())
      )
    )
  )

}



