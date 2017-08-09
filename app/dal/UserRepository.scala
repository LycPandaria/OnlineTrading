package dal

/**
  * Created by lyc08 on 2017/6/27.
  */

import java.sql.{SQLException, Timestamp}
import javax.inject.{Inject, Singleton}

import models.{Order, User}
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import util.Calculator

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

@Singleton
class UserRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {

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

  private class UserTable(tag: Tag) extends Table[User](tag, "user") {
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
  private class OrderTable(tag: Tag) extends Table[Order](tag, "order") {

    /** The ID column, which is the primary key, and auto incremented */
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def timestamp = column[Timestamp]("timestamp")
    def tradetype = column[String]("tradetype")
    def price = column[Double]("price")
    def amount = column[Double]("amount")
    def outstanding = column[Double]("outstanding")
    def status = column[String]("status")
    def uid = column[Long]("uid")
    def ordertype = column[String]("ordertype")

    /**
      * This is the tables default "projection".
      *
      * It defines how the columns are converted to and from the  object.
      *
      * In this case, we are simply passing the id, name and page parameters to the Person case classes
      * apply and unapply methods.
      */
    def * = (id, timestamp, tradetype, price, amount, outstanding, status, uid, ordertype) <> ((Order.apply _).tupled, Order.unapply)
  }
  private val orders = TableQuery[OrderTable]

  def add(user: User): Future[Int] = {
    db.run(users += user).map {
      res => SUCCESS
    }.recover {
      case ex: Exception =>
        println(ex.getMessage)
        UNKNOWN
    }
  }

  /**
    *
    * @param username    username
    * @param md5Password user's password, hashed by md5 at client
    * @return the user with specific username and password or None if not found
    */
  def login(username: String, md5Password: String): Future[Option[User]] = {
    db.run(users.filter(_.username === username).filter(_.password === md5Password).take(1).result.headOption)
  }

  /**
    *
    * @param username    - username
    * @param md5Password -user's password, hashedby by md5 at client
    * @param email       - user's email
    * @return - return the id of the added user
    */
  def register(username: String, md5Password: String, email: String): Future[Long] = {
    findByUsername(username).flatMap {
      user =>
        if (user.isDefined)
          Future {
            0L
          }
        else
          db.run((users returning users.map(_.id)) += User(0, username, md5Password, email, 0.0, 0.0, 0.0, 0.0))
    }
  }

  /**
    *
    * @param username
    * @return user with username or none
    */
  def findByUsername(username: String): Future[Option[User]] = db.run {
    users.filter(_.username === username).result.headOption
  }

  def findById(uid: Long): Future[Option[User]] = db.run {
    users.filter(_.id === uid).result.headOption
  }

  def update(user: User): Future[Int] = {
    db.run {
      users.filter(_.id === user.id).update(user).map {
        res => SUCCESS
      }
    }.recover {
      case ex: Exception =>
        println(ex.getMessage)
        FAIL
    }
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
    if (amount > user.gbpBalance)
      Future.successful(NOT_ENOUGH_GBP_BALANCE)
    else {
      val newBalance = Calculator.sub(user.gbpBalance, amount)
      val newUser = user.copy(gbpBalance = newBalance)
      update(newUser)
    }
  }

  def btcWithdrawal(amount: Double, user: User): Future[Int] = {
    if (amount > user.btcBalance)
      Future.successful(NOT_ENOUGH_BTC_BALANCE)
    else {
      val newBalance = Calculator.sub(user.gbpBalance, amount)
      val newUser = user.copy(btcBalance = newBalance)
      update(newUser)
    }
  }

  def updateBalance(uid: Long, order: Order): Long = {
      order.tradetype match {
        case BUY =>
          // buy btc, so we have to deduct GBP from user account
          val totalPrice = Calculator.toFixed(Calculator.multi(order.amount, order.price))
          val gbpR_query = users.filter(_.id === uid).map(_.gbpReserved)
          val gbpB_query = users.filter(_.id === uid).map(_.gbpBalance)

          val action = for {
            id <- (orders returning orders.map(_.id)) += order
            gbpB <- gbpB_query.result.head
            gbpR <- gbpR_query.result.head
            res1 <- if (totalPrice <= gbpB)
              DBIO.seq(gbpB_query.update(Calculator.sub(gbpB, totalPrice)),
                gbpR_query.update(Calculator.add(gbpR, totalPrice)))
            else DBIO.failed(new SQLException("Abort"))
          } yield id

          Await.result(db.run(action.transactionally).map {
            id => id
          }.recover {
            case e: java.sql.SQLException =>
              println("Caught exception in the for block: " + e.getMessage)
              0L
          }, 5.second)

        case SELL =>
          val btcB_query = users.filter(_.id === uid).map(_.btcBalance)
          val btcR_query = users.filter(_.id === uid).map(_.btcReserved)

          val action = for {
            id <- (orders returning orders.map(_.id)) += order
            btcB <- btcB_query.result.head
            btcR <- btcR_query.result.head
            res <-
            if (order.amount <= btcB)
              DBIO.seq(btcB_query.update(Calculator.sub(btcB, order.amount)),
                btcR_query.update(Calculator.add(btcR, order.amount)))
            else DBIO.failed(new SQLException("Abort"))
          } yield id

          Await.result(db.run(action.transactionally).map {
            id => id
          }.recover {
            case e: java.sql.SQLException =>
              println("Caught exception in the for block: " + e.getMessage)
              0L
          }, 5.second)
      }

  }

  def updateTest(x: Double): Future[Int] = {

    val action = for {
      gbpB <- users.filter(_.id === 3L).map(_.gbpBalance).result.head
      res1 <- users.filter(_.id === 3L).map(_.gbpBalance).update(Calculator.sub(gbpB, 2000))
    } yield (gbpB, res1)


    val run = db.run(action.transactionally).map {
      case (gbpB, res1) =>
        println(s"gbpB: $gbpB")
        SUCCESS
    }.recover {
      case e: java.sql.SQLException =>
        println("Caught exception in the for block: " + e.getMessage)
        NOT_ENOUGH_GBP_BALANCE
    }
    run
  }

}
