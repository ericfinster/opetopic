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
      UUID.randomUUID(),
      user.userID,
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

}
