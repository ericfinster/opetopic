/**
  * SketchController.scala - Controller for the Sketchpad
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package controllers

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.Future

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.impl.authenticators.CookieAuthenticator
import play.api.i18n.{ MessagesApi, Messages }
import play.api.libs.concurrent.Execution.Implicits._

import models.User
import models.Sketch
import models.services.UserService
import models.daos.SketchDAO

import upickle.default._

import opetopic._
import opetopic.net._

class SketchController @Inject() (
  val messagesApi: MessagesApi,
  val env: Environment[User, CookieAuthenticator],
  userService: UserService,
  sketchDAO: SketchDAO
) extends Silhouette[User, CookieAuthenticator] {

  def sketchpad = UserAwareAction.async { implicit request => 

    request.identity match {
      case Some(user) => 
        sketchDAO.userSketches(user).map { sketches =>
          Ok(views.html.sketchpad(sketches)(Some(user)))
        }
      case None => Future.successful {
        Ok(views.html.sketchpad(Seq())(None))
      }
    }

  }

  def getSketch = SecuredAction.async { implicit request => 

    request.body.asText.map { text =>

      val req = read[LoadSketchRequest](text)

      sketchDAO.getSketch(UUID.fromString(req.id)).map { 
        case None => Ok("Not found")
        case Some(sketch) => {
          // Right, you should do better, no?
          Ok(sketch.data)
        }
      }

    } getOrElse Future.successful(BadRequest("Bad sketch request"))

  }


  def saveSketch = SecuredAction.async { implicit request => 

    request.body.asText.map { text => 

      import opetopic.ui.markers._
      import SimpleMarker._

      val req = read[SaveSketchRequest](text)

      // The read here is just to check that it parses.  Probable
      // we should catch any exception and return a bad result.
      val fc : FiniteComplex[OptMarker] =
        Complex.fromJson[OptMarker](upickle.json.read(req.data))

      println("Read a complex: " ++ fc.value.toString)

      val sketch = Sketch(
        UUID.randomUUID(),
        request.identity.userID,
        req.name,
        req.path,
        req.description,
        req.data
      )

      for {
        _ <- sketchDAO.save(request.identity, sketch)
      } yield Ok("Save complete")

    } getOrElse Future.successful(BadRequest("Bad save request"))

  }

}
