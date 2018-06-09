/**
  * SketchController.scala - Controller for the Sketchpad
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package controllers

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.{Future, ExecutionContext}

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.impl.authenticators.CookieAuthenticator
import play.api.mvc.{ AbstractController, AnyContent, ControllerComponents }
import play.api.i18n.{ MessagesApi, Messages }
import play.api.i18n.I18nSupport
import play.Environment
import org.webjars.play.WebJarsUtil
import utils.auth.DefaultEnv

import models.User
import models.Sketch
import models.services.UserService
import models.daos.SketchDAO
import forms.RenderSketchForm

import upickle.default._

import opetopic._
import opetopic.ui._
import opetopic.net._

/**
 * The SketchPad controller
 *
 * @param components  The Play controller components.
 * @param silhouette  The Silhouette stack.
 * @param sketchDAO   The Sketch data access object
 * @param webJarsUtil The webjar util.
 * @param assets      The Play assets finder.
 */
class SketchController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],
  sketchDAO: SketchDAO,
  env: Environment
)(
  implicit
  webJarsUtil: WebJarsUtil,
  assets: AssetsFinder,
  ec: ExecutionContext
) extends AbstractController(components) with I18nSupport {

  def sketchpad = silhouette.UserAwareAction.async { implicit request => 

    request.identity match {
      case Some(user) => 
        sketchDAO.userSketches(user).map { sketches =>
          Ok(views.html.sketchpad(RenderSketchForm.form, sketches, env.isDev)(request, Some(user), webJarsUtil))
        }
      case None => Future.successful {
        Ok(views.html.sketchpad(RenderSketchForm.form, Seq(), env.isDev)(request, None, webJarsUtil))
      }
    }

  }

  def getSketch = silhouette.SecuredAction.async { implicit request => 

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

  def deleteSketch = silhouette.SecuredAction.async { implicit request => 

    request.body.asText.map { text => 

      val req = read[DeleteSketchRequest](text)

      sketchDAO.deleteSketch(UUID.fromString(req.id)).map {
        (i: Int) => Ok("Deleted " + i.toString + " sketch")
      }

    } getOrElse Future.successful(BadRequest("Bad delete request"))

  }

  def saveSketch = silhouette.SecuredAction.async { implicit request => 

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

  def renderSketch = silhouette.UserAwareAction.async { implicit request => 

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

  def renderAddrProof = silhouette.UserAwareAction.async { implicit request =>

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
