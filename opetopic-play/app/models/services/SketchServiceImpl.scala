/**
  * SketchServiceImpl.scala - Sketch Serivice Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models.services

import java.util.UUID
import javax.inject.Inject

import models.User
import models.sketchpad.Sketch

import models.daos.SketchDAO

class SketchServiceImpl @Inject() (sketchDAO: SketchDAO) extends SketchService {

  def save(user: User, sketch: Sketch) = sketchDAO.save(user, sketch)

}
