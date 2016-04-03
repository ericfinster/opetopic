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
import models.sketchpad.Sketch
import models.services.UserService
import models.services.SketchService

import scala.concurrent.Future

class SketchController @Inject() (
  val messagesApi: MessagesApi,
  val env: Environment[User, CookieAuthenticator],
  userService: UserService,
  sketchService: SketchService
) extends Silhouette[User, CookieAuthenticator] {

  def saveSketch = SecuredAction.async { implicit request => 

    request.body.asText.map { text => 

      import opetopic._
      import opetopic.net._
      import opetopic.ui.markers._
      import SimpleMarker._

      import upickle.default._

      val req = read[SaveSketchRequest](text)

      // The read here is just to check that it parses.  Probable
      // we should catch any exception and return a bad result.
      val fc : FiniteComplex[OptMarker] =
        Complex.fromJson[OptMarker](upickle.json.read(req.data))

      println("Read a complex: " ++ fc.value.toString)

      val sketch = 
        Sketch(req.name, req.path, req.description, req.data)

      for {
        _ <- sketchService.save(request.identity, sketch)
      } yield Ok("Save complete")

    } getOrElse Future.successful(BadRequest("Bad save request"))

  }

}
