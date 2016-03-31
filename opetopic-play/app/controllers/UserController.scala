/**
  * UserController.scala - Handle Access to the user database
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext

import play.api._
import play.api.mvc._

import play.api.db.slick._
import slick.driver.PostgresDriver.api._
import slick.driver.JdbcProfile

import opetopic.net._

import scala.concurrent._
import scala.concurrent.duration._

import scala.util.Try
import scala.util.Success
import scala.util.Failure

object UserApi extends OpetopicApi {

  // Grab a slick config handle to the user database
  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  class Users(tag: Tag) extends Table[(Int, String, String)](tag, "users") {

    def id = column[Int]("id")
    def username = column[String]("username")
    def passhash = column[String]("passhash")

    def * = (id, username, passhash)

  }

  def createUser(id: String, passwd: String) : String = {

    val users = TableQuery[Users]

    val insertFuture : Future[Option[Int]] = 
      dbConfig.db.run(users ++= Seq((3, id, passwd)))

    val insertTry : Try[Option[Int]] = 
      Await.ready(insertFuture, Duration.Inf).value.get

    insertTry match {
      case Success(_) => "Created user: " + id
      case Failure(_) => "Error creating user"
    }

  }

  def listUsers() : Seq[String] = {

    val users = TableQuery[Users]

    val readbackFuture : Future[Seq[(Int, String, String)]] = 
      dbConfig.db.run(users.result)

    val readbackTry : Try[Seq[(Int, String, String)]] = 
      Await.ready(readbackFuture, Duration.Inf).value.get

    readbackTry match {
      case Success(seq) => for { s <- seq } yield s._2
      case Failure(e) => Seq()
    }

  //   val allUsers = dbConfig.db.run(users.result)
  //   allUsers.map(users => {

  //     var out = "User List:\n" + "----------\n"

  //     users.foreach{
  //       case (id, username, _) => out += "user with id " + id + " has username " + username + "\n"
  //     }

  //     Ok(out)

  //   })


  // val people = TableQuery[Persons]
  //     val setupAction : DBIO[Unit] = DBIO.seq(
  //       people.schema.create
  //       )
  //     val setupFuture : Future[Unit] = db.run(setupAction)

  //       val populateFuture : Future[Option[Int]] = db.run(populateAction)

  //       populateFuture.map {results =>
  //         results.foreach(x => println(s"Number of rows inserted $x"))
  //       }
  //     }

  }


// val result: Try[T] = Await.ready(f, Duration.Inf).value.get

// val resultEither = result match {
//   case Success(t) => Right(t)
//   case Failure(e) => Left(e)
// }

  // def userlist = Action.async { implicit request =>


  // }

  // Some query examples ...

  // def index(name: String) = Action.async { implicit request =>
  //   val resultingUsers: Future[Seq[User]] = dbConfig.db.run(Users.filter(_.name === name).result)
  //   resultingUsers.map(users => Ok(views.html.index(users)))
  // }

  // // SELECT * FROM users
  // users.list foreach { row =>
  //   println("user with id " + row._1 + " has username " + row._2)
  // }

  // // SELECT * FROM users WHERE username='john'
  // users.filter(_.username === "john").list foreach { row =>
  //   println("user whose username is 'john' has id "+row._1 )
  // }



}
