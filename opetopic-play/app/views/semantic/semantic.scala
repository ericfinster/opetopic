/**
  * semantic.scala - Custom FieldConstructor for SemantiUI
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package views.html.semantic

object semantic {

  import views.html.helper.FieldConstructor

  implicit val semanticFields = 
    FieldConstructor(textinput.f)

}
