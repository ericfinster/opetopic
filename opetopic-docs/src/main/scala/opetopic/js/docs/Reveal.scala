/**
  * Reveal.scala - Minimal facade for working with reveal.js
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.docs

import scala.scalajs._
import org.scalajs.dom

// Yeah, these are just dummies.  Don't really know
// what they types should be ...
@js.native
trait RevealEvent extends js.Object {
  def previousSlide: dom.Element = js.native
  def currentSlide: dom.Element = js.native
  def indexh: Int = js.native
  def indexv: Int = js.native
  def fragment: dom.Element = js.native
}


@js.native
object Reveal extends js.Object {

  def initialize(): Unit = js.native
  def initialize(obj: js.Object): Unit = js.native

  def configure(obj: js.Object): Unit = js.native

  def addEventListener(event: String, handler: js.Function1[RevealEvent, js.Any], b: Boolean): Unit = js.native

}
