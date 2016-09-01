/**
  * ApplicationController.scala - Main Application Controller
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package controllers

import javax.inject.Inject

import com.mohiva.play.silhouette.api.{ Environment, LogoutEvent, Silhouette }
import com.mohiva.play.silhouette.impl.authenticators.CookieAuthenticator
import com.mohiva.play.silhouette.impl.providers.SocialProviderRegistry
import forms._
import models.User
import play.api.i18n.MessagesApi

import scala.concurrent.Future

/**
 * The basic application controller.
 *
 * @param messagesApi The Play messages API.
 * @param env The Silhouette environment.
 * @param socialProviderRegistry The social provider registry.
 */
class ApplicationController @Inject() (
  val messagesApi: MessagesApi,
  val env: Environment[User, CookieAuthenticator],
  socialProviderRegistry: SocialProviderRegistry)
  extends Silhouette[User, CookieAuthenticator] {

  /**
   * Handles the index action.
   *
   * @return The result to display.
   */
  def index = UserAwareAction.async { implicit request =>
    Future.successful(Ok(views.html.index()(request.identity)))
  }

  def stable = UserAwareAction.async { implicit request =>
    implicit val uo = request.identity
    Future.successful(Ok(views.html.stable()))
  }

  // Temporarily put the multi-editor here
  def multiedit = UserAwareAction.async { implicit request =>
    Future.successful(Ok(views.html.multiedit()(request.identity)))
  }

  /**
   * Handles the Sign In action.
   *
   * @return The result to display.
   */
  def signIn = UserAwareAction.async { implicit request =>

    request.identity match {
      case Some(user) => Future.successful(Redirect(routes.ApplicationController.index()))
      case None => {
        implicit val userOpt: Option[User] = None
        Future.successful(Ok(views.html.signIn(SignInForm.form, socialProviderRegistry)))
      }
    }

  }

  /**
   * Handles the Sign Up action.
   *
   * @return The result to display.
   */
  def signUp = UserAwareAction.async { implicit request =>

    request.identity match {
      case Some(user) => Future.successful(Redirect(routes.ApplicationController.index()))
      case None => {
        implicit val userOpt: Option[User] = None
        Future.successful(Ok(views.html.signUp(SignUpForm.form)))
      }
    }

  }

  /**
   * Handles the Sign Out action.
   *
   * @return The result to display.
   */
  def signOut = SecuredAction.async { implicit request =>
    val result = Redirect(routes.ApplicationController.index())
    env.eventBus.publish(LogoutEvent(request.identity, request, request2Messages))
    env.authenticatorService.discard(request.authenticator, result)
  }

}
