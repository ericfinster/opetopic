/**
  * Module.scala - A class holding a module
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scala.collection.mutable.ListBuffer

import org.scalajs.dom.Element
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic.ott.TypeChecker._

class Module(val name: String) { thisModule =>

  var moduleId: Option[String] = None
  var description: String = ""
  var code: String = ""
  var isLoaded: Boolean = false
  var isTypechecked: Boolean = false

  var moduleEnv: Option[TCEnv] = None

}
