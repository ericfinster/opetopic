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
import forms.RenderSketchForm

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
          Ok(views.html.sketchpad(RenderSketchForm.form, sketches)(request, Some(user)))
        }
      case None => Future.successful {
        Ok(views.html.sketchpad(RenderSketchForm.form, Seq())(request, None))
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

  def renderSketch = UserAwareAction.async { implicit request => 

    RenderSketchForm.form.bindFromRequest.fold(
      form => Future.successful(BadRequest("Bad render reqeust")),
      data => {

        import opetopic.ui._
        import opetopic.ui.markers._
        import ScalatagsTextFramework._
        import SimpleMarker._

        implicit val staticPanelConfig =
          PanelConfig(
            internalPadding = 400,
            externalPadding = 600,
            decorationPadding = 400,
            leafWidth = 200,
            strokeWidth = 100,
            cornerRadius = 200
          )

        implicit val staticGalleryConfig =
          GalleryConfig(
            panelConfig = staticPanelConfig,
            width = 1000,
            height = 300,
            spacing = 2000,
            minViewX = Some(80000),
            minViewY = Some(15000),
            spacerBounds = Bounds(0, 0, 600, 600)
          )

        implicit val spacerBounds = Bounds(0, 0, 600, 600)

        implicit val vf: VisualizableFamily[SimpleMarker] =
          frameworkFamily(ScalatagsTextFramework)

        val fc : FiniteComplex[OptMarker] =
          Complex.fromJson[OptMarker](upickle.json.read(data.renderData))

        println("Rendering complex: " ++ fc.value.toString)

        val staticGallery = SimpleStaticGallery(fc)
        val xmlHeader: String = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
        Future.successful(Ok(xmlHeader + "\n" + staticGallery.element.toString).as("image/svg+xml"))

      }
    )

  }

}
