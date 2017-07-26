package dal

/**
  * Created by lyc08 on 2017/6/27.
  */

import javax.inject.{Inject, Singleton}

import models.{Order, User}
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import util.Calculator

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UserRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext){

  // We want the JdbcProfile for this provider
  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  val BUY = "buy"
  val SELL = "sell"
  val GBP = "GBP"
  val BTC = "BTC"
  val NO_SUCH_USER = 2
  val NOT_ENOUGH_GBP_BALANCE = 0
  val NOT_ENOUGH_BTC_BALANCE: Int = 3
  val SUCCESS = 1
  val UNKNOWN = 4
  val FAIL = 5

  import dbConfig._
  import driver.api._

  private class UserTable(tag: Tag) extends Table[User](tag, "user"){
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def username = column[String]("username")
    def password = column[String]("password")
    def email = column[String]("email")
    def btcBalance = column[Double]("btc_balance")
    def gbpBalance = column[Double]("gbp_balance")
    def btcReserved = column[Double]("btc_reserved")
    def gbpReserved = column[Double]("gbp_reserved")

    def * = (id, username, password, email, btcBalance, gbpBalance, btcReserved, gbpReserved) <> ((User.apply _).tupled, User.unapply)
  }

  private val users = TableQuery[UserTable]

  def add(user: User): Future[Int] = {
    db.run(users+=user).map{
      res => SUCCESS
    }.recover{
      case ex: Exception =>
        println(ex.getMessage)
        UNKNOWN
    }
  }

  /**
    *
    * @param username     username
    * @param md5Password  user's password, hashed by md5 at client
    * @return             the user with specific username and password or None if not found
    */
  def login(username: String, md5Password: String): Future[Option[User]] = {
    db.run(users.filter(_.username === username).filter(_.password === md5Password).take(1).result.headOption)
  }

  /**
    *
    * @param username - username
    * @param md5Password  -user's password, hashedby by md5 at client
    * @param email  - user's email
    * @return - return the id of the added user
    */
  def register(username: String, md5Password: String, email: String):Future[Long] ={
    findByUsername(username).flatMap{
      user =>
        if(user.isDefined)
          Future{0L}
        else
          db.run((users returning users.map(_.id)) += User(0, username, md5Password, email, 0.0, 0.0, 0.0, 0.0))
    }
  }

  /**
    *
    * @param username
    * @return user with username or none
    */
  def findByUsername(username: String): Future[Option[User]] = db.run{
    users.filter(_.username === username).result.headOption
  }

  def findById(uid: Long): Future[Option[User]] = db.run{
    users.filter(_.id === uid).result.headOption
  }

  def update(user: User): Future[Int] = db.run{
    users.filter(_.id === user.id).update(user).map{
      res => SUCCESS
    }
  }.recover{
    case ex: Exception =>
      println(ex.getMessage)
      FAIL
  }

  def gbpTopup(amount: Double, user: User): Future[Int] = {
    val newBalance = Calculator.add(user.gbpBalance, amount)
    val newUser = user.copy(gbpBalance = newBalance)
    update(newUser)
  }

  def btcTopup(amount: Double, user: User): Future[Int] = {
    val newBlance = Calculator.add(user.btcBalance, amount)
    val newUser = user.copy(btcBalance = newBlance)
    update(newUser)
  }

  def gbpWithdrawal(amount: Double, user: User): Future[Int] = {
    if(amount > user.gbpBalance)
      Future.successful(NOT_ENOUGH_GBP_BALANCE)
    else{
      val newBalance = Calculator.sub(user.gbpBalance, amount)
      val newUser = user.copy(gbpBalance = newBalance)
      update(newUser)
    }
  }

  def btcWithdrawal(amount: Double, user: User): Future[Int] = {
    if(amount > user.btcBalance)
      Future.successful(NOT_ENOUGH_BTC_BALANCE)
    else{
      val newBalance = Calculator.sub(user.gbpBalance, amount)
      val newUser = user.copy(btcBalance = newBalance)
      update(newUser)
    }
  }

  def updateBalance(user: User, order: Order): Future[Int] = {
    order.tradetype match {
      case BUY =>
        // buy btc, so we have to deduct GBP from user account
        val totalPrice = Calculator.toFixed(Calculator.multi(order.amount, order.price))
        if(totalPrice <= user.gbpBalance){
          val left_balance = Calculator.sub(user.gbpBalance, totalPrice)
          val left_reserved = Calculator.add(user.gbpReserved, totalPrice)
          val u = user.copy(gbpBalance = left_balance, gbpReserved = left_reserved)
          update(u)
        } else
          Future.successful(NOT_ENOUGH_GBP_BALANCE)
      case SELL =>
        // sell btc, so we have to deduct BTC from user account
        if(order.amount <= user.btcBalance){
          val left_balance = Calculator.sub(user.btcBalance, order.amount)
          val left_reserved = Calculator.add(user.btcReserved, order.amount)
          val u = user.copy(btcBalance = left_balance, btcReserved =left_reserved)
          update(u)
        }else
          Future.successful(NOT_ENOUGH_BTC_BALANCE)
    }
  }

}
