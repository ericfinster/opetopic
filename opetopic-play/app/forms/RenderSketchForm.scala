/**
  * RenderSketchForm.scala - Request form for rendering sketches
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package forms

import play.api.data.Form
import play.api.data.Forms._

object RenderSketchForm {

  val form = Form(
    mapping(
      "fileName" -> nonEmptyText, 
      "renderData" -> nonEmptyText,
      "sizingMethod" -> nonEmptyText
    )(Data.apply)(Data.unapply)
  )

  case class Data(
    fileName: String, 
    renderData: String,
    sizingMethod: String
  )

}
