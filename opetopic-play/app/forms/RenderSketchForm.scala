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
      "renderData" -> nonEmptyText
    )(Data.apply)(Data.unapply)
  )

  case class Data(
    renderData: String
  )

}
