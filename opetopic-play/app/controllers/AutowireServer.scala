/**
  * AutowireServer.scala - Server side autowire implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future

import play.api._
import play.api.mvc._

import upickle.Js
import upickle.default._

import opetopic.net._

object AutowireController extends Controller with autowire.Server[Js.Value, Reader, Writer] {

  def run(str: String) = Action.async { implicit request => 

    request.body.asText map { (e: String) => 

      route[OpetopicApi](UserApi)(
        autowire.Core.Request(
          str.split("/"),
          upickle.json.read(e).asInstanceOf[Js.Obj].value.toMap
        )
      ).map((v: Js.Value) =>
        Ok(upickle.json.write(v, 0))
      )

    } getOrElse Future { BadRequest("Could not parse string data") }

  }

  def read[Result: Reader](p: Js.Value) = readJs[Result](p)
  def write[Result: Writer](r: Result) = writeJs(r)

}
