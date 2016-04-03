/**
  * SketchController.scala - Controller for the Sketchpad
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package controllers

import java.util.UUID
import javax.inject.Inject

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.impl.authenticators.CookieAuthenticator
import play.api.i18n.{ MessagesApi, Messages }
import play.api.libs.concurrent.Execution.Implicits._

import models.User
import models.services.UserService

import scala.concurrent.Future

class SketchController @Inject() (
  val messagesApi: MessagesApi,
  val env: Environment[User, CookieAuthenticator],
  userService: UserService
) extends Silhouette[User, CookieAuthenticator] {

  def saveSketch = SecuredAction.async { implicit request => 

    import opetopic._
    import opetopic.ui.markers._
    import SimpleMarker._

    request.body.asText.map { json => {

      println("Json: " ++ json)

      val fc : FiniteComplex[OptMarker] = 
        Complex.fromJson[OptMarker](upickle.json.read(json))

      val sketch = Sketch(fc)

      println("Result: " ++ fc.value.toString)

    }}


    Future.successful(Ok("Saving a sketch ..."))

  }

}
