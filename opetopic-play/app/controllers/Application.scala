package controllers

import play.api.libs.concurrent.Execution.Implicits.defaultContext

import play.api._
import play.api.mvc._

import play.api.db.slick._
import slick.driver.PostgresDriver.api._
import slick.driver.JdbcProfile

import libs.json._

object Application extends Controller {

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  def index = Action {
    Ok(views.html.index())
  }

  def sketchpad = Action {
    Ok(views.html.sketchpad())
  }

  def prover = Action {
    Ok(views.html.prover())
  }

  def showDoc(page: String) = Action {
    page match {
      case "intro" => Ok(views.html.docs.intro())
      case "typetheory" => Ok(views.html.docs.typetheory())
      case "hdts" => Ok(views.html.docs.hdts())
      case "opetopes" => Ok(views.html.docs.opetopes())
      case _ => Ok("Not found: " ++ page)
    }
  }

  def showTutorial(page: String) = Action {
    page match {
      case "intro" => Ok(views.html.tutorial.intro())
      case "liftings" => Ok(views.html.tutorial.liftings())
      case _ => Ok("Not found: " ++ page)
    }
  }


  class Users(tag: Tag) extends Table[(Int, String)](tag, "userstest") {

    def id = column[Int]("id")
    def username = column[String]("username")

    def * = (id, username)

  }

  def db = Action.async { implicit request =>

    val users = TableQuery[Users]

    val allUsers = dbConfig.db.run(users.result)
    allUsers.map(users => {

      var out = ""
      users.foreach{
        case (id, username) => out += "user with id " + id + " has username " + username + "\n"
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
