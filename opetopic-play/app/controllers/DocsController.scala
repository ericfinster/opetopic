/**
  * DocsController.scala - Documentation Controller
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package controllers

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.{Future, ExecutionContext}

import com.mohiva.play.silhouette.api._
import play.api.mvc.{ AbstractController, AnyContent, ControllerComponents }
import play.api.i18n.{ MessagesApi, Messages }
import play.api.i18n.I18nSupport
import play.Environment
import org.webjars.play.WebJarsUtil
import utils.auth.DefaultEnv

import models.User
import models.services.UserService

class DocsController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],
  env: Environment
)(
  implicit
  webJarsUtil: WebJarsUtil,
  assets: AssetsFinder,
  ec: ExecutionContext
) extends AbstractController(components) with I18nSupport {

  def showDoc(page: String) = silhouette.UserAwareAction.async { implicit request => 

    implicit val userOpt = request.identity

    page match {
      case "intro" => Future.successful(Ok(views.html.docs.intro(env.isDev)))
      case "typetheory" => Future.successful(Ok(views.html.docs.typetheory(env.isDev)))
      case "hdts" => Future.successful(Ok(views.html.docs.hdts(env.isDev)))
      case "diagrams/complexes" => Future.successful(Ok(views.html.docs.complexes(env.isDev)))
      case "diagrams/opetopes" => Future.successful(Ok(views.html.docs.opetopes(env.isDev)))
      case "diagrams/geometry" => Future.successful(Ok(views.html.docs.geometry(env.isDev)))
      case "diagrams/osets" => Future.successful(Ok(views.html.docs.osets(env.isDev)))
      case "categories/extrusions" => Future.successful(Ok(views.html.docs.extrusions(env.isDev)))
      case "categories/uprops" => Future.successful(Ok(views.html.docs.uprops(env.isDev)))
      case "categories/categories" => Future.successful(Ok(views.html.docs.categories(env.isDev)))
      case "theory/units" => Future.successful(Ok(views.html.docs.units(env.isDev)))
      case "theory/eqvs" => Future.successful(Ok(views.html.docs.eqvs(env.isDev)))
      case "theory/srccoh" => Future.successful(Ok(views.html.docs.srccoh(env.isDev)))
      case "basicediting" => Future.successful(Ok(views.html.docs.basicediting(env.isDev)))
      case _ => Future.successful(Ok("No doc found"))
    }

  }

}

