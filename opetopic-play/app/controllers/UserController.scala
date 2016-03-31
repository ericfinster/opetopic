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

object UserController extends Controller {

  // Grab a slick config handle to the user database
  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  class Users(tag: Tag) extends Table[(Int, String, String)](tag, "users") {

    def id = column[Int]("id")
    def username = column[String]("username")
    def passhash = column[String]("passhash")

    def * = (id, username, passhash)

  }

  def userlist = Action.async { implicit request =>

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
