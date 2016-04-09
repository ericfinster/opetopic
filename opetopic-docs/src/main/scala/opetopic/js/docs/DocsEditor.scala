/**
  * DocsEditor.scala - An Editor Implementation for the Documentation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.docs

import org.scalajs.dom
import org.scalajs.jquery._

import opetopic._
import opetopic.js._
import opetopic.ui._
import markers._
import JsDomFramework._
import SimpleMarker._

class DocsEditor extends JsCardinalEditor[SimpleMarker] {

  implicit val vf: VisualizableFamily[SimpleMarker] = 
    SimpleMarker.frameworkFamily(JsDomFramework)

}
