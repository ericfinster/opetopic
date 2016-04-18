/**
  * Snap.scala - Minimal facade for working with Snap.svg
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.docs

import scala.scalajs._
import org.scalajs.dom

@js.native
object Snap extends js.Object {

  def apply(width: Double, height: Double) : SnapElement = js.native
  def apply(el: dom.Element) : SnapElement = js.native
  def apply(sel: String) : SnapElement = js.native

  // Actually, I think you get the XMLHttpRequest back ...
  def load(url: String, cb: js.Function1[Fragment, Unit]) : Unit = js.native 

}

@js.native
trait Fragment extends js.Object {

  def select(sel: String) : SnapElement = js.native
  def selectAll(sel: String) : SnapSet = js.native

}

@js.native
trait SnapElement extends js.Object {

  def append(els: SnapSet) : Unit = js.native
  def append(f: Fragment) : Unit = js.native

  def select(sel: String) : SnapElement = js.native
  def selectAll(sel: String) : SnapSet = js.native

  def attr(o: js.Object) : Unit = js.native
  def attr(name: String) : js.Any = js.native

  def transform(tstr: String) : Unit = js.native

  def animate(o: js.Object, d: Int) : Unit = js.native

}

@js.native
trait SnapSet extends js.Object {

  def attr(o: js.Object) : Unit = js.native
  def attr(name: String) : js.Any = js.native

  def animate(o: js.Object, d: Int) : Unit = js.native

}
