/**
  * ProverController.scala - Controller for the Opetopic Prover
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
import models.daos.ProverDAO

import upickle.default._

import opetopic._
import opetopic.net._

class ProverController @Inject() (
  val messagesApi: MessagesApi,
  val env: Environment[User, CookieAuthenticator],
  userService: UserService,
  proverDAO: ProverDAO
) extends Silhouette[User, CookieAuthenticator] {

  def prover = SecuredAction.async { implicit request => 

    proverDAO.userModules(request.identity).map { modules =>
      Ok(views.html.prover(modules))
    }

  }

}
