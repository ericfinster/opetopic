package controllers

import play.api._
import play.api.mvc._

import libs.json._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }

  def sketchpad = Action {
    Ok(views.html.sketchpad())
  }

  def codeBuilder = Action {
    Ok(views.html.codebuilder())
  }

  def layout = Action {
    Ok(views.html.layout())
  }

  def docs = Action {
    Ok(views.html.docs())
  }

}
