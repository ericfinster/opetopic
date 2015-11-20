/**
  * JQuerySemanticUI.scala - JQuery Facade for SemanticUI functions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs._
import org.scalajs.jquery._

@js.native
trait JQuerySemanticUI extends JQuery {

  def dropdown(opts: js.Object) : this.type = js.native
  def modal(opts: js.Object) : this.type = js.native
  def modal(str: String) : this.type = js.native
  def sidebar(str: String) : this.type = js.native

}

object JQuerySemanticUI {
  implicit def jq2semqntic(jq: JQuery): JQuerySemanticUI =
    jq.asInstanceOf[JQuerySemanticUI]
}
