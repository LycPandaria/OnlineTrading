package dal

import java.sql.Timestamp
import javax.inject.{Inject, Singleton}

import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import models.{Order, OrderForm}
import slick.profile.SqlProfile.ColumnOption.SqlType

import scala.concurrent.{ExecutionContext, Future}

/**
  * A repository for order
  *
  * @param dbConfigProvider The Play db config provider. Play will inject this for you.
  */
@Singleton
class OrderRepository @Inject() (dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {
  // We want the JdbcProfile for this provider
  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  val BUY = "buy"
  val SELL = "sell"
  val STATUS_CLOSED = "closed"
  val STATUS_OPEN = "open"

  // These imports are important, the first one brings db into scope, which will let you do the actual db operations.
  // The second one brings the Slick DSL into scope, which lets you define the table and other queries.
  import dbConfig._
  import driver.api._

  /**
    * Here we define the table. It will have a name of order
    */
  private class OrderTable(tag: Tag) extends Table[Order](tag, "order") {

    /** The ID column, which is the primary key, and auto incremented */
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def timestamp = column[Timestamp]("timestamp")
    def tradetype = column[String]("tradetype")
    def price = column[Double]("price")
    def amount = column[Double]("amount")
    def outstanding = column[Double]("outstanding")
    def status = column[String]("status")

    /**
      * This is the tables default "projection".
      *
      * It defines how the columns are converted to and from the  object.
      *
      * In this case, we are simply passing the id, name and page parameters to the Person case classes
      * apply and unapply methods.
      */
    def * = (id, timestamp, tradetype, price, amount, outstanding, status) <> ((Order.apply _).tupled, Order.unapply)
  }

  /**
    * The starting point for all queries on the people table.
    */
  private val orders = TableQuery[OrderTable]

  /**
    * List all the orders in the database.
    */
  def list(tradetype: String): Future[Seq[Order]] =  {
    tradetype match {
      case BUY => db.run {
        // list all buy order, sort by price(desc) and timestamp
        // select * from order where tradetype='buy' and status='open' orderby price DESC, timestamp
        orders.filter(_.status === STATUS_OPEN).filter(_.tradetype === BUY)
          .sortBy(row => (row.price.desc, row.timestamp)).result
      }
      case SELL => db.run{
        // select * from order where tradetype='sell' and status='open' orderby price ASC, timestamp
        orders.filter(_.status === STATUS_OPEN).filter(_.tradetype === SELL)
          .sortBy(row => (row.price.asc, row.timestamp)).result
      }
    }
  }

  /**
    * group price into (tradetype = 'buy', price, total_amount)
    */
  def listCombinedOrders(tradetype: String): Future[Seq[OrderForm]] = {
    tradetype match{
      case BUY => db.run {
        /* this query is equal to
        select tradetype, price, sum(outstanding) from order
        where tradetype='buy'and status='open'
        group by price
        order by price
        */
        val query = orders.filter(_.status === STATUS_OPEN).filter(_.tradetype === BUY)
          .groupBy(_.price).map{
          case(price, group) => (BUY, price, group.map(_.outstanding).sum)
        }.sortBy(_._2.desc)   // sort by price in DESC

        // query will results a Seq[(String, Double, Option[Double])]
        query.result.map{
          seq =>  // type of seq: Seq[(String, Double, Option[Double])]
            // turn this into a Seq[OrderForm]
            seq.map( data => OrderForm(BUY, data._2, data._3.get))
        }
      } // end of BUY
      case SELL => db.run {
        /*  this query is equal to
            select tradetype, price, sum(outstanding) from order
            where tradetype='sell' and status='open'
            group by price
            order by price
        */
        val query = orders.filter(_.status === STATUS_OPEN).filter(_.tradetype === SELL)
          .groupBy(_.price).map{
          case(price, group) => (SELL, price, group.map(_.outstanding).sum)
        }.sortBy(_._2.asc)   // sort by price in ASC

        // query will results a Seq[(String, Double, Option[Double])]
        query.result.map{
          seq =>  // type of seq: Seq[(String, Double, Option[Double])]
            // turn this into a Seq[OrderForm]
            seq.map( data => OrderForm(SELL, data._2, data._3.get))
        }
      } // end of SELL
    } // end of match
  }



  def add(order: Order) = {
    // add a new buy order
    println("add a new order")
    // search for suitable sell orders
    findSuitableOrders(order).onSuccess{
      case suitableOrders =>
        if(suitableOrders.isEmpty){
          // if no suitable orders to trade with this order, add order to DB
          println("add to DB directly")
          addOrderToDB(order)
        }
        else{
          println("Do trade trasaction")
          trade(order, suitableOrders)
        }
    }
  }

  /**
  select * from order
  where tradetype=order.tradetype and price = order.price and status='open'
  order by price ASC, timestamp
   */
  def findSuitableOrders(order: Order):Future[Seq[Order]] = {
    order.tradetype match {
      case BUY =>
        val query = orders.filter(_.status === STATUS_OPEN).filter(_.tradetype === SELL).filter(_.price === order.price)
          .sortBy(_.timestamp)

        db.run(query.result).map{
          list =>
            // these code will filter some orders
            // if some one want to buy amount=0.1 and the first suitable's amount = 0.2, this function will only return
            // the first order as suitable. otherwise, function will continue to next order
            var amount: Double = 0.0
            var x: Seq[Order] = Seq()
            for(o <- list){
              if(amount <= order.amount){
                amount+=o.outstanding
                x = x :+ o
              }
            }
            x
        }

      case SELL =>
        val query = orders.filter(_.status === STATUS_OPEN).filter(_.tradetype === BUY).filter(_.price === order.price)
          .sortBy(_.timestamp)
        db.run(query.result).map{
          list =>
            // these code will filter some orders
            // if some one want to buy amount=0.1 and the first suitable's amount = 0.2, this function will only return
            // the first order as suitable. otherwise, function will continue to next order
            var amount: Double = 0.0
            var x: Seq[Order] = Seq()
            for(o <- list){
              if(amount <= order.amount){
                amount+=o.outstanding
                x = x :+ o
              }
            }
            x
        }
    }
  }
  /**
    * add an order to database
    */
  def addOrderToDB(order: Order):Future[Int] = {
    db.run(orders+=order)
  }

  /**
    * update an existed order
    * @param order order to be updated
    * @return
    */
  def updateOrderToDB(order: Order): Future[Int] = {
    db.run{
      orders.filter(_.id === order.id).update(order)
    }
  }

  /**
    * Doing a trade action between a new order with suitable orders
    * @param order  the new order
    * @param ordersToTrade orders which are suitable to trade with the new order
    */
  def trade(order: Order, ordersToTrade: Seq[Order]) = {
    // sum of outstanding of suitable orders
    val amount = ordersToTrade.map(_.outstanding).sum

    import scala.math.BigDecimal
    if (amount == order.amount){
      println("money excahnge:" + amount * order.price)
      // ordersToTrade will be updated to close
      // new order added to DB with status=closed
      val newOrder = order.copy(status = STATUS_CLOSED)
      // add to DB
      addOrderToDB(newOrder)
      println("Order to be added:" + newOrder)
      ordersToTrade.foreach{
        o =>
          val update = o.copy(status = STATUS_CLOSED)
          // update to DB
          updateOrderToDB(update)
          println("Order to be updated:" + update)
      }
    }
    else if(amount < order.amount){
      // left amount in the new order
      val outstanding_BD= BigDecimal(order.amount.toString) - BigDecimal(amount.toString)
      val outstanding = outstanding_BD.toString().toDouble

      // update all ordersToTrade to close
      ordersToTrade.foreach{
        o =>
          val update = o.copy(status = STATUS_CLOSED)
          // update to DB
          updateOrderToDB(update)
          println("Order to be updated:" + update)
      }

      // add new order to DB with this outstanding value
      val newOrder = order.copy(outstanding = outstanding)
      // add to DB
      addOrderToDB(newOrder)
      println("Order to be added:" + newOrder)
    }
    else{ // amount > order.amount
      val outstanding_BD= BigDecimal(amount.toString) - BigDecimal(order.amount.toString)
      val outstanding = outstanding_BD.toString().toDouble
      // new order will be closed
      val newOrder = order.copy(status = STATUS_CLOSED)
      // add to DB
      println("Order to be added:" + newOrder)
      addOrderToDB(newOrder)

      // the last order in ordersToTrade will remain open with new outstanding, others will be closed
      for(i <- 0 to ordersToTrade.size - 2){
        // update others to closed
        val close = ordersToTrade(i).copy(status = STATUS_CLOSED)
        println("Order to be updated (closed):" + close)
        updateOrderToDB(close)
      }
      val update = ordersToTrade.last.copy(outstanding = outstanding)
      println("Order to be updated:" + update)
      updateOrderToDB(update)
    }
  }
}

