/**
  * DocsController.scala - Documentation Controller
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
import models.services.UserService

class DocsController @Inject() (
  val messagesApi: MessagesApi,
  val env: Environment[User, CookieAuthenticator],
  userService: UserService
) extends Silhouette[User, CookieAuthenticator] {


  def showDoc(page: String) = UserAwareAction.async { implicit request => 

    implicit val userOpt = request.identity

    page match {
      case "intro" => Future.successful(Ok(views.html.docs.intro()))
      case "typetheory" => Future.successful(Ok(views.html.docs.typetheory()))
      case "hdts" => Future.successful(Ok(views.html.docs.hdts()))
      case "diagrams/complexes" => Future.successful(Ok(views.html.docs.complexes()))
      case "diagrams/opetopes" => Future.successful(Ok(views.html.docs.opetopes()))
      case "diagrams/geometry" => Future.successful(Ok(views.html.docs.geometry()))
      case "diagrams/osets" => Future.successful(Ok(views.html.docs.osets()))
      case "categories/extrusions" => Future.successful(Ok(views.html.docs.extrusions()))
      case "categories/uprops" => Future.successful(Ok(views.html.docs.uprops()))
      case "categories/categories" => Future.successful(Ok(views.html.docs.categories()))
      case "theory/units" => Future.successful(Ok(views.html.docs.units()))
      case "theory/eqvs" => Future.successful(Ok(views.html.docs.eqvs()))
      case "theory/srccoh" => Future.successful(Ok(views.html.docs.srccoh()))
      case "basicediting" => Future.successful(Ok(views.html.docs.basicediting()))
      case _ => Future.successful(Ok("No doc found"))
    }

  }

  def tutorial = UserAwareAction.async { 
    Future.successful(Ok(views.html.docs.tutorial()))
  }

}
