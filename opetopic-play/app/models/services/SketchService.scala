/**
  * SketchService.scala - The Sketch Service
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models.services

import scala.concurrent.Future

import models.User
import models.sketchpad.Sketch

trait SketchService {

  def save(user: User, sketch: Sketch) : Future[Sketch]

}
