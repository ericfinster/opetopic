/**
  * ProverDAOImpl.scala - The Prover DAO Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models.daos

import java.util.UUID
import models.User
import models.Module
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import slick.dbio.DBIOAction
import javax.inject.Inject
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.Future

class ProverDAOImpl @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends ProverDAO with DAOSlick {

  import driver.api._

  def userModules(user: User) : Future[Seq[Module]] = {

    val action = 
      slickModules.filter(_.authorId === user.userID).result

    db.run(action).map { dbModules =>
      for { m <- dbModules } yield
        Module(m.moduleId, m.authorId, m.name, m.description getOrElse "", m.data)
    }

  }

  def saveModule(module: Module) : Future[Module] = {

    val dbModule = DBModule(
      module.moduleId,
      module.authorId,
      module.name,
      if (module.description == "") None else Some(module.description),
      module.data
    )

    val action = (for {
      _ <- slickModules.insertOrUpdate(dbModule)
    } yield ()).transactionally

    db.run(action).map(_ => module)
    
  }

}
