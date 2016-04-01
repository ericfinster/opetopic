/**
  * UserController.scala - Handle Access to the user database
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package controllers

import play.api._
import play.api.mvc._

import play.api.db.slick._
import slick.driver.PostgresDriver.api._
import slick.driver.JdbcProfile

import play.api.libs.concurrent.Execution.Implicits.defaultContext

import opetopic.net._

import upickle.Js
import upickle.default._

import scala.concurrent._
import scala.util.Success
import scala.util.Failure

object UserController extends Controller {

  // Grab a slick config handle to the user database
  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  class Users(tag: Tag) extends Table[(Int, String, String)](tag, "users") {

    def id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    def username = column[String]("username")
    def passhash = column[String]("passhash")

    def * = (id, username, passhash)

  }

  // def createUser(id: String, passwd: String) : String = {

  //   val users = TableQuery[Users]

  //   val insertFuture : Future[Option[Int]] = 
  //     dbConfig.db.run(users ++= Seq((0, id, passwd)))

  //   val insertTry : Try[Option[Int]] = 
  //     Await.ready(insertFuture, Duration.Inf).value.get

  //   insertTry match {
  //     case Success(_) => "Created user: " + id
  //     case Failure(_) => "Error creating user"
  //   }

  // }

  // def listUsers() : Seq[String] = {

  //   val users = TableQuery[Users]

  //   val readbackFuture : Future[Seq[(Int, String, String)]] = 
  //     dbConfig.db.run(users.result)

  //   val readbackTry : Try[Seq[(Int, String, String)]] = 
  //     Await.ready(readbackFuture, Duration.Inf).value.get

  //   readbackTry match {
  //     case Success(seq) => for { s <- seq } yield s._2
  //     case Failure(e) => Seq()
  //   }


  // }

  def createUser = Action.async { implicit request =>

    request.body.asText map { (e: String) =>

      val users = TableQuery[Users]
      val req = read[CreateUserReq](e)

      dbConfig.db.run(
        users ++= Seq((4, req.username, req.password))
      ).map(_ => Ok("Created user: " ++ req.username))

    } getOrElse Future {
      BadRequest("Invalid user creation request") 
    }

  }

  def userList = Action.async { implicit request =>

    val users = TableQuery[Users]

    val allUsers = dbConfig.db.run(users.result)
    allUsers.map(users => {

      var out = "User List:\n" + "----------\n"

      users.foreach{
        case (id, username, _) => out += "user with id " + id + " has username " + username + "\n"
      }

      Ok(out)

    })

  }



}
