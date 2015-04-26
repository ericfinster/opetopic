package controllers

import play.api._
import play.api.mvc._

import libs.json._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("This is some content which goes in a panel."))
  }

  def debug = Action(BodyParsers.parse.json) { request =>
    Logger.debug("Received a json post:")
    Logger.debug(Json.prettyPrint(request.body))
    Ok(Json.obj("status" -> "OK", "message" -> "all done"))
  }

}
