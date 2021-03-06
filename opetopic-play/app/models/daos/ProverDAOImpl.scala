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
import slick.dbio.DBIOAction
import javax.inject.Inject
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.{Future, ExecutionContext}

class ProverDAOImpl @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext)
    extends ProverDAO with DAOSlick {

  import profile.api._

  def userModules(user: User) : Future[Seq[Module]] = {

    val action = 
      slickModules.filter(_.authorId === user.userID).result

    db.run(action).map { dbModules =>
      for { m <- dbModules } yield
        Module(m.moduleId, m.authorId, m.name, m.description getOrElse "", m.data)
    }

  }

  def getModule(uuid: UUID) : Future[Option[Module]] = {

    val action =
      slickModules.filter(_.moduleId === uuid) 

    db.run(action.result.headOption).map { dbModuleOpt =>
      dbModuleOpt map { m =>
        Module(m.moduleId, m.authorId, m.name, m.description getOrElse "", m.data)
      }
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

  def deleteModule(moduleId: UUID): Future[Int] = {

    val action = 
      slickModules.filter(_.moduleId === moduleId).delete

    db.run(action)

  }

}
