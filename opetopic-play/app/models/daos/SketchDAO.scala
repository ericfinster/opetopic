/**
  * SketchDAO.scala - Data Access Object for Sketches
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models.daos

import java.util.UUID
import models.User
import models.Sketch
import scala.concurrent.Future

trait SketchDAO {

  def save(user: User, sketch: Sketch) : Future[Sketch] 
  def userSketches(user: User) : Future[Seq[Sketch]]
  def getSketch(sketchId: UUID) : Future[Option[Sketch]]

}
