package dal

import java.sql.Timestamp
import javax.inject.{Inject, Singleton}

import models.Trade
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}
/**
  * Created by lyc08 on 2017/7/5.
  */
@Singleton
class TradeRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext){
  // We want the JdbcProfile for this provider
  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import driver.api._

  private class TradeTable(tag: Tag) extends Table[Trade](tag, "trade") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def timestamp = column[Timestamp]("timestamp")
    def tradePrice = column[Double]("trade_price")
    def amount = column[Double]("amount")
    def total = column[Double]("total")
    def tradeType = column[String]("trade_type")
    def traderId = column[Long]("trader_id")
    def tradeeId = column[Long]("tradee_id")
    def orderId = column[Long]("initiator_order_id")
    def orderTradeWithId = column[Long]("trade_order_id")

    def * = (id, timestamp, tradePrice, amount,total, tradeType, traderId, tradeeId, orderId, orderTradeWithId) <> ((Trade.apply _).tupled, Trade.unapply)
  }

  val NO_SUCH_USER = 2
  val NOT_ENOUGH_GBP_BALANCE = 0
  val NOT_ENOUGH_BTC_BALANCE: Int = 3
  val SUCCESS = 1
  val UNKNOWN = 4
  val FAIL = 5

  private val trades = TableQuery[TradeTable]

  def add(trade: Trade): Future[Int] = {
    db.run{trades+=trade}.map{
      res => SUCCESS
    }.recover{
      case ex: Exception =>
        println(ex.getMessage)
        FAIL
    }
  }

  def getLastPrice(): Future[Double] = {
    db.run{trades.sortBy(_.timestamp.desc).result.headOption}.map{
      case Some(t) => t.tradePrice
      case None => 0.0
    }
  }

  def list(): Future[Seq[Trade]] = {
    db.run{
      trades.sortBy(_.timestamp.desc).take(10).result
    }
  }

  def findTradesByUid(uid: Long): Future[Seq[Trade]]= {
    db.run{
      trades.filter(row => row.tradeeId === uid || row.traderId === uid).sortBy(_.timestamp.desc).result
    }
  }

}
