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
import opetopic.ui._
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

  def deleteSketch = SecuredAction.async { implicit request => 

    request.body.asText.map { text => 

      val req = read[DeleteSketchRequest](text)

      sketchDAO.deleteSketch(UUID.fromString(req.id)).map {
        (i: Int) => Ok("Deleted " + i.toString + " sketch")
      }

    } getOrElse Future.successful(BadRequest("Bad delete request"))

  }

  def saveSketch = SecuredAction.async { implicit request => 

    request.body.asText.map { text => 

      val req = read[SaveSketchRequest](text)
      val c = complexFromJson[Option[SimpleMarker]](upickle.json.read(req.data))

      println("Read a complex: " ++ c.toString)

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

        import ScalatagsTextFramework._

        val c = complexFromJson[Option[SimpleMarker]](upickle.json.read(data.renderData))

        val staticGallery = new SimpleStaticGallery(ScalatagsTextFramework)(c)

        val maxWidth = 725
        val maxHeight = 260

        val fct = 0.02
        staticGallery.layoutWidth = (b: Bounds) => { val fw = (b.width * fct).toInt ; if (fw > maxWidth) maxWidth else fw }
        staticGallery.layoutHeight = (b: Bounds) => { val fh = (b.height * fct).toInt ; if (fh > maxHeight) maxHeight else fh }

        val xmlHeader: String = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
        Future.successful(Ok(xmlHeader + "\n" + staticGallery.element.toString).as("image/svg+xml"))

      }
    )

  }

  def renderAddrProof = UserAwareAction.async { implicit request =>

    RenderSketchForm.form.bindFromRequest.fold(
      form => Future.successful(BadRequest("Bad render reqeust")),
      data => {

        import ScalatagsTextFramework._

        val c = complexFromJson[Option[SimpleMarker]](upickle.json.read(data.renderData))
        val addrCmplx : SComplex[SAddr] = c.mapWithAddr({
          case (_, fa) => fa.address
        })

        val staticGallery = new SimpleStaticGallery(ScalatagsTextFramework)(addrCmplx)

        val fct = 0.02
        staticGallery.layoutWidth = (b: Bounds) => { (b.width * fct).toInt }
        staticGallery.layoutHeight = (b: Bounds) => { (b.height * fct).toInt }

        val xmlHeader: String = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
        Future.successful(Ok(xmlHeader + "\n" + staticGallery.element.toString).as("image/svg+xml"))

      }
    )

  }

}
