/**
  * SketchDAOImpl.scala - Sketch Data Access Object Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models.daos

import java.util.UUID
import models.User
import models.sketchpad.Sketch
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import slick.dbio.DBIOAction
import javax.inject.Inject
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.Future

class SketchDAOImpl @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends SketchDAO with DAOSlick {

  import driver.api._

  def save(user: User, sketch: Sketch) : Future[Sketch] = {

    val dbSketch = DBSketch(
      sketch.sketchId,
      sketch.authorId,
      sketch.name,
      if (sketch.path != "") Some(sketch.path) else None,
      if (sketch.description != "") Some(sketch.description) else None,
      sketch.data
    )

    val actions = (for {
      _ <- slickSketches.insertOrUpdate(dbSketch)
    } yield ()).transactionally

    db.run(actions).map(_ => sketch)

  }

  def userSketches(user: User) : Future[Seq[Sketch]] = {

    val action = 
      slickSketches.filter(_.authorId === user.userID).result

    db.run(action).map { dbSketches =>
      for { s <- dbSketches } yield
        Sketch(s.sketchId, s.authorId, s.name, s.description getOrElse "", s.path getOrElse "/", s.data)
    }

  }

  def getSketch(sketchId: UUID) : Future[Option[Sketch]] = {

    val action =
      slickSketches.filter(_.sketchId === sketchId) 

    db.run(action.result.headOption).map { dbSketchOpt =>
      dbSketchOpt map { s =>
        Sketch(s.sketchId, s.authorId, s.name, s.description getOrElse "", s.path getOrElse "/", s.data)
      }
    }

  }

}
