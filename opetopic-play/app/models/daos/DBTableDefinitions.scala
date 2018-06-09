/**
  * DBTableDefinitions.scala - Database Table Definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models.daos

import java.util.UUID

import com.mohiva.play.silhouette.api.LoginInfo
import slick.jdbc.JdbcProfile
import slick.lifted.ProvenShape.proveShapeOf

trait DBTableDefinitions {
  
  protected val driver: JdbcProfile
  import driver.api._

  case class DBUser (
    userID: UUID,
    firstName: Option[String],
    lastName: Option[String],
    fullName: Option[String],
    email: Option[String],
    avatarURL: Option[String]
  )

  class Users(tag: Tag) extends Table[DBUser](tag, "users") {
    def id = column[UUID]("userid", O.PrimaryKey)
    def firstName = column[Option[String]]("firstname")
    def lastName = column[Option[String]]("lastname")
    def fullName = column[Option[String]]("fullname")
    def email = column[Option[String]]("email")
    def avatarURL = column[Option[String]]("avatarurl")
    def * = (id, firstName, lastName, fullName, email, avatarURL) <> (DBUser.tupled, DBUser.unapply)
  }

  case class DBLoginInfo (
    id: Option[Long],
    providerID: String,
    providerKey: String
  )

  class LoginInfos(tag: Tag) extends Table[DBLoginInfo](tag, "logininfo") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def providerID = column[String]("providerid")
    def providerKey = column[String]("providerkey")
    def * = (id.?, providerID, providerKey) <> (DBLoginInfo.tupled, DBLoginInfo.unapply)
  }

  case class DBUserLoginInfo (
    userID: UUID,
    loginInfoId: Long
  )

  class UserLoginInfos(tag: Tag) extends Table[DBUserLoginInfo](tag, "userlogininfo") {
    def userID = column[UUID]("userid")
    def loginInfoId = column[Long]("logininfoid")
    def * = (userID, loginInfoId) <> (DBUserLoginInfo.tupled, DBUserLoginInfo.unapply)
  }

  case class DBPasswordInfo (
    hasher: String,
    password: String,
    salt: Option[String],
    loginInfoId: Long
  )

  class PasswordInfos(tag: Tag) extends Table[DBPasswordInfo](tag, "passwordinfo") {
    def hasher = column[String]("hasher")
    def password = column[String]("password")
    def salt = column[Option[String]]("salt")
    def loginInfoId = column[Long]("logininfoid")
    def * = (hasher, password, salt, loginInfoId) <> (DBPasswordInfo.tupled, DBPasswordInfo.unapply)
  }

  case class DBOAuth1Info (
    id: Option[Long],
    token: String,
    secret: String,
    loginInfoId: Long
  )

  class OAuth1Infos(tag: Tag) extends Table[DBOAuth1Info](tag, "oauth1info") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def token = column[String]("token")
    def secret = column[String]("secret")
    def loginInfoId = column[Long]("logininfoid")
    def * = (id.?, token, secret, loginInfoId) <> (DBOAuth1Info.tupled, DBOAuth1Info.unapply)
  }

  case class DBOAuth2Info (
    id: Option[Long],
    accessToken: String,
    tokenType: Option[String],
    expiresIn: Option[Int],
    refreshToken: Option[String],
    loginInfoId: Long
  )

  class OAuth2Infos(tag: Tag) extends Table[DBOAuth2Info](tag, "oauth2info") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def accessToken = column[String]("accesstoken")
    def tokenType = column[Option[String]]("tokentype")
    def expiresIn = column[Option[Int]]("expiresin")
    def refreshToken = column[Option[String]]("refreshtoken")
    def loginInfoId = column[Long]("logininfoid")
    def * = (id.?, accessToken, tokenType, expiresIn, refreshToken, loginInfoId) <> (DBOAuth2Info.tupled, DBOAuth2Info.unapply)
  }
  
  case class DBOpenIDInfo (
    id: String,
    loginInfoId: Long
  )
  
  class OpenIDInfos(tag: Tag) extends Table[DBOpenIDInfo](tag, "openidinfo") {
    def id = column[String]("id", O.PrimaryKey)
    def loginInfoId = column[Long]("logininfoid")
    def * = (id, loginInfoId) <> (DBOpenIDInfo.tupled, DBOpenIDInfo.unapply)
  }
  
  case class DBOpenIDAttribute (
    id: String,
    key: String,
    value: String
  )
  
  class OpenIDAttributes(tag: Tag) extends Table[DBOpenIDAttribute](tag, "openidattributes") {
    def id = column[String]("id")
    def key = column[String]("key")
    def value = column[String]("value")
    def * = (id, key, value) <> (DBOpenIDAttribute.tupled, DBOpenIDAttribute.unapply)
  }

  //============================================================================================
  // SKETCH TABLES
  //

  case class DBSketch(
    sketchId: UUID,
    authorId: UUID,
    name: String,
    path: Option[String],
    description: Option[String],
    data: String
  )

  class Sketches(tag: Tag) extends Table[DBSketch](tag, "sketches") {
    def sketchId = column[UUID]("sketchid", O.PrimaryKey)
    def authorId = column[UUID]("authorid")
    def name = column[String]("name")
    def path = column[Option[String]]("path")
    def description = column[Option[String]]("description")
    def data = column[String]("data")
    def * = (sketchId, authorId, name, path, description, data) <> (DBSketch.tupled, DBSketch.unapply)
  }

  //============================================================================================
  // PROVER TABLES
  //

  case class DBModule(
    moduleId: UUID,
    authorId: UUID,
    name: String,
    description: Option[String],
    data: String
  )

  class Modules(tag: Tag) extends Table[DBModule](tag, "modules") {
    def moduleId = column[UUID]("moduleid", O.PrimaryKey)
    def authorId = column[UUID]("authorid")
    def name = column[String]("name")
    def description = column[Option[String]]("description")
    def data = column[String]("data")
    def * = (moduleId, authorId, name, description, data) <> (DBModule.tupled, DBModule.unapply)
  }

  // table query definitions
  val slickUsers = TableQuery[Users]
  val slickLoginInfos = TableQuery[LoginInfos]
  val slickUserLoginInfos = TableQuery[UserLoginInfos]
  val slickPasswordInfos = TableQuery[PasswordInfos]
  val slickOAuth1Infos = TableQuery[OAuth1Infos]
  val slickOAuth2Infos = TableQuery[OAuth2Infos]
  val slickOpenIDInfos = TableQuery[OpenIDInfos]
  val slickOpenIDAttributes = TableQuery[OpenIDAttributes]

  val slickSketches = TableQuery[Sketches]
  val slickModules = TableQuery[Modules]

  // queries used in multiple places
  def loginInfoQuery(loginInfo: LoginInfo) = 
    slickLoginInfos.filter(dbLoginInfo => dbLoginInfo.providerID === loginInfo.providerID && dbLoginInfo.providerKey === loginInfo.providerKey)

}
