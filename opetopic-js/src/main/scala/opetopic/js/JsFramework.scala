/**
  * JsFramework.scala - Javascript Visual Framework Backend
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs._
import org.scalajs.dom

import opetopic.vf.backend._

class JsFramework extends ScalaTagsFramework(scalatags.JsDom) {

  // The thing we are going to have to do is to attach an event handler
  // to the main page and start to process events.

  // I don't see much choice: the framework needs to start up and grab the
  // event system of the document. This should be at a kind of global level.

  // From there, this class will have instances of components, and these can
  // be created at any point.  But the problem is how does one then know which
  // components are attached to the document.  There doesn't seem to be like
  // any notion of a scene graph.

  // This is not quite true: when render is called, we will essentially trace 
  // through all the components which we are dependent on.

  // Mmmm ... 

  // This is where you have this weird distinction that scalajs-react seems to
  // make: when you render, do you return the component itself, or a rendering
  // of it's internals.

  // I think the point is that you return the component.  This automatically
  // builds a kind of scene graph: for the base component, the result of the
  // render method are its children, and so on.

  

}


