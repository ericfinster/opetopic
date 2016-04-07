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
import models.Module
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

  def prover = UserAwareAction.async { implicit request => 

    request.identity match {
      case Some(user) => 
        proverDAO.userModules(user).map { modules =>
          Ok(views.html.prover(modules)(Some(user)))
        }
      case None => Future.successful {
        Ok(views.html.prover(Seq())(None))
      }
    }

  }

  def getModule = SecuredAction.async { implicit request => 

    request.body.asText.map { text =>

      val req = read[LoadModuleRequest](text)

      proverDAO.getModule(UUID.fromString(req.uuid)).map { 
        case None => BadRequest("Not found")
        case Some(module) => Ok(module.data)
      }

    } getOrElse Future.successful(BadRequest("Bad module request"))

  }

  def saveModule = SecuredAction.async { implicit request =>

    request.body.asText.map { text => 

      val req = read[SaveModuleRequest](text)

      println("Saving module: " + req.name)
      println(req.data)

      val moduleId = 
        req.moduleId match {
          case None => UUID.randomUUID()
          case Some(uuid) => UUID.fromString(uuid)
        }

      val module = Module(
        moduleId,
        request.identity.userID,
        req.name,
        req.description,
        req.data
      )

      for {
        m <- proverDAO.saveModule(module)
      } yield Ok(m.moduleId.toString)

    } getOrElse Future.successful(BadRequest("Bad save module request"))

  }

}
