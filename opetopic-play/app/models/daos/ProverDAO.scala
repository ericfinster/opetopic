/**
  * ProverDAO.scala - The Data Access Object for the Prover
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models.daos

import java.util.UUID
import models.User
import models.Module
import scala.concurrent.Future

trait ProverDAO {

  def userModules(user: User) : Future[Seq[Module]]

}
