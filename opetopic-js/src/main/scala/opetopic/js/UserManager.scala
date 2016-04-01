/**
  * UserManager.scala - Manage Client-side user information
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.concurrent.Future
import scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.scalajs.dom.XMLHttpRequest
import org.scalajs.dom.ext.Ajax

import upickle.default._
import opetopic.net._

object UserManager {

  def createUser(username: String, passwd: String) : Unit = {

    println("Sending create user request: " ++ username)

    val req : Future[String] = 
      Ajax.post(
        url = "/createuser",
        data = write(CreateUserReq(username, passwd))
      ).map(_.responseText)

    req.onSuccess {
      case msg => println("Received message: " ++ msg)
    }

  }


}
