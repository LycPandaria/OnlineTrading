package dal

import java.sql.Timestamp
import javax.inject.{Inject, Singleton}

import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import models.{Order, OrderForm, Trade, User}
import util.Calculator

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}

/**
  * A repository for order
  *
  * @param dbConfigProvider The Play db config provider. Play will inject this for you.
  */
@Singleton
class OrderRepository @Inject() (userRepo: UserRepository, tradeRepo: TradeRepository, dbConfigProvider: DatabaseConfigProvider)
                                (implicit ec: ExecutionContext) {
  // We want the JdbcProfile for this provider
  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  val BUY = "buy"
  val SELL = "sell"
  val LIMIT = "limit"
  val MARKET = "market"
  val STATUS_CLOSED = "closed"
  val STATUS_OPEN = "open"
  val NO_SUCH_USER = 2
  val NOT_ENOUGH_GBP_BALANCE = 0
  val NOT_ENOUGH_BTC_BALANCE: Int = 3
  val SUCCESS = 1
  val UNKNOWN = 4
  val FAIL = -1

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

  /**
    * The starting point for all queries on the people table.
    */
  private val orders = TableQuery[OrderTable]


  /**
    * group price into (tradetype = 'buy', price, total_amount)
    */
  def listCombinedOrders(tradetype: String, limit: Int): Future[Seq[OrderForm]] = {
    tradetype match{
      case BUY => db.run {
        /* this query is equal to
        select tradetype, price, sum(outstanding) from order
        where tradetype='buy'and status='open'
        group by price
        order by price
        */
        val query = orders.filter(r => r.status===STATUS_OPEN && r.tradetype===BUY && r.ordertype===LIMIT)
          .groupBy(_.price).map{
          case(price, group) => (BUY, price, group.map(_.outstanding).sum)
        }.sortBy(_._2.desc)   // sort by price in DESC

        // query will results a Seq[(String, Double, Option[Double])]
        query.result.map{
          seq =>  // type of seq: Seq[(String, Double, Option[Double])]
            // turn this into a Seq[OrderForm]
            seq.map( data => OrderForm(BUY, data._2, Calculator.toFixed(data._3.get))).take(limit)
        }
      } // end of BUY
      case SELL => db.run {
        /*  this query is equal to
            select tradetype, price, sum(outstanding) from order
            where tradetype='sell' and status='open'
            group by price
            order by price
        */
        val query = orders.filter(r => r.status===STATUS_OPEN && r.tradetype===SELL && r.ordertype===LIMIT)
          .groupBy(_.price).map{
          case(price, group) => (SELL, price, group.map(_.outstanding).sum)
        }.sortBy(_._2.asc)   // sort by price in ASC

        // query will results a Seq[(String, Double, Option[Double])]
        query.result.map{
          seq =>  // type of seq: Seq[(String, Double, Option[Double])]
            // turn this into a Seq[OrderForm]
            seq.map( data => OrderForm(SELL, data._2, Calculator.toFixed(data._3.get))).take(limit)
        }
      } // end of SELL
    } // end of match
  }

  /**
    * find order by id
    * @param id
    * @return
    */
  def findById(id: Long): Future[Option[Order]] = {
    db.run(orders.filter(_.id === id).result.headOption)
  }

  /**
    * find order by uid
    * @param uid
    * @return
    */
  def findOrdersByUid(uid: Long): Future[Seq[Order]] = {
    db.run{
      orders.filter(_.uid === uid).sortBy(_.timestamp.desc).result
    }
  }

  /**
    * find the max buy price
    * select price from order where status=open and type=buy sort by price desc (select the first one row)
    * @return
    */
  def maxBuyPrice(): Future[Double] = {
    db.run{
      orders.filter(row => row.status===STATUS_OPEN && row.tradetype===BUY)
        .sortBy(_.price.desc).result.headOption
    }.map{
      case Some(order) => order.price
      case None => 0.0
    }
  }

  /**
    * find the min sell price
    * select price from order where status=open and type=sell sort by price asc (select the first one row)
    * @return
    */
  def minSellPrice(): Future[Double] = {
    db.run{
      orders.filter(row => row.status===STATUS_OPEN && row.tradetype===SELL)
        .sortBy(_.price.asc).result.headOption
    }.map{
      case Some(order) => order.price
      case None => 0.0
    }
  }

  /**
    * add an order to database
    */
  def addOrderToDB(order: Order):Future[Long] = {
    db.run((orders returning orders.map(_.id)) += order).recover{
      case ex: Exception =>
        println(ex.getMessage)
        0L
    }
  }

  def addOrderToDBblocking(order: Order): Long = {
    Await.result(db.run((orders returning orders.map(_.id)) += order).recover{
      case ex: Exception =>
        println(ex.getMessage)
        0L
    }, Duration.Inf)
  }

  /**
    * update an existed order
    * @param order order to be updated
    * @return
    */
  def updateOrderToDB(order: Order): Future[Int] = {
    db.run{
      orders.filter(_.id === order.id).update(order)
    }.map{
      res => SUCCESS
    }.recover{
      case ex: Exception =>
        println(ex.getMessage)
        FAIL
    }
  }

  def add(order: Order): Int = {
    blocking {
      this.synchronized {
        // add to order db and update user
        userRepo.updateBalance(order.uid, order) match {
          case 0L => FAIL
          case id =>
            val orderToTrade = order.copy(id = id)
            trade(orderToTrade)
        }
      }
    }
  }


  /**
  select * from order
  where tradetype=order.tradetype and price <= order.price and status='open'
  order by price ASC, timestamp
   */
  def findSuitableOrders(order: Order):Seq[Order] = {
      order.tradetype match {
        case BUY =>
          val query = orders.filter(_.status === STATUS_OPEN).filter(_.tradetype === SELL).filter(_.price <= order.price)
            .sortBy(row => (row.price.asc, row.timestamp))

          Await.result(db.run(query.result).map {
            list =>
              // these code will filter some orders
              // if some one want to buy amount=0.1 and the first suitable's amount = 0.2, this function will only return
              // the first order as suitable. otherwise, function will continue to next order
              var amount: Double = 0.0
              var x: Seq[Order] = Seq()
              for (o <- list) {
                if (amount < order.amount) {
                  amount += o.outstanding
                  x = x :+ o
                }
              }
              x
          }, Duration.Inf)
        case SELL =>
          val query = orders.filter(_.status === STATUS_OPEN).filter(_.tradetype === BUY).filter(_.price >= order.price)
            .sortBy(row => (row.price.desc, row.timestamp))
          Await.result(db.run(query.result).map {
            list =>
              // these code will filter some orders
              // if some one want to buy amount=0.1 and the first suitable's amount = 0.2, this function will only return
              // the first order as suitable. otherwise, function will continue to next order
              var amount: Double = 0.0
              var x: Seq[Order] = Seq()
              for (o <- list) {
                if (amount < order.amount) {
                  amount += o.outstanding
                  x = x :+ o
                }
              }
              x
          }, Duration.Inf)
      }

  }


  /**
    * Doing a trade action between a new order with suitable orders
    * @param order  the new order
    *
    */
  def trade(order: Order): Int = {
    val ordersToTrade = findSuitableOrders(order)
      var code = SUCCESS
      // trade one by one
      ordersToTrade.foreach {
        o =>
          val trade =
            for {
              trader <- userRepo.findById(order.uid)
              orderToTrade <- findById(order.id)
              tradee <- userRepo.findById(o.uid)
              res <- tradeWithOneOrder(trader.get, orderToTrade.get, tradee.get, o)
            } yield res
          code = Await.result(trade, Duration.Inf)
      }
      code

  }


  /**
    * Check whether the user has enough balance to trade, so the system can lock those currency
    * @param user
    * @param order
    * @return
    */
  def checkBalance(user: User, order: Order): Int = {
    order.tradetype match {
      case BUY =>
        // get the total price of this order
        val totalPrice = Calculator.multi(order.price, order.amount)
        if(user.gbpBalance >= totalPrice)
        // can trade
          SUCCESS
        else
          NOT_ENOUGH_GBP_BALANCE
      case SELL =>
        if(user.btcBalance >= order.amount)
          SUCCESS
        else
          NOT_ENOUGH_BTC_BALANCE
    }
  }

  /**
    * Trade operation
    * @param trader user who initiate this trade
    * @param order  the order the trader initiate
    * @param tradee user to trade with
    * @param orderTradeWith the order to trade with the incoming order
    * @return
    */
  def tradeWithOneOrder(trader: User, order: Order, tradee: User, orderTradeWith: Order): Future[Int] = {
      var midPrice, diffPrice, total, refund, amount = 0.0
      order.tradetype match {
        case BUY =>
          if (order.outstanding >= orderTradeWith.outstanding) {
            midPrice = Calculator.midprice(order.price, orderTradeWith.price) // mid price
            diffPrice = Calculator.sub(order.price, midPrice).abs
            amount = orderTradeWith.outstanding
            total = Calculator.toFixed(Calculator.multi(midPrice, amount))
            refund = Calculator.toFixed(Calculator.multi(diffPrice, amount))

            // update user
            val traderGBPB = Calculator.add(trader.gbpBalance, refund)
            val traderGBPR = Calculator.sub(trader.gbpReserved, Calculator.add(total, refund))
            val tradeeGBPB = Calculator.add(tradee.gbpBalance, total)
            val traderBTCB = Calculator.add(trader.btcBalance, amount)
            val tradeeBTCR = Calculator.sub(tradee.btcReserved, amount)

            val traderUpdate =
              if (trader.id == tradee.id)
                trader.copy(gbpBalance = Calculator.add(tradeeGBPB, refund), gbpReserved = traderGBPR,
                  btcBalance = traderBTCB, btcReserved = tradeeBTCR)
              else
                trader.copy(gbpBalance = traderGBPB, gbpReserved = traderGBPR, btcBalance = traderBTCB)
            val tradeeUpdate =
              if (trader.id == tradee.id)
                traderUpdate
              else
                tradee.copy(gbpBalance = tradeeGBPB, btcReserved = tradeeBTCR)

            // update order
            val tradedOrder = orderTradeWith.copy(outstanding = 0.0, status = STATUS_CLOSED)
            val orderWithNewStatus =
              Calculator.sub(order.outstanding, amount) match {
                case 0.0 => order.copy(outstanding = 0.0, status = STATUS_CLOSED)
                case left => order.copy(outstanding = left)
              }
            val update = for {
            //update to database
              res1 <- userRepo.update(traderUpdate)
              res2 <- userRepo.update(tradeeUpdate)
              res3 <- updateOrderToDB(tradedOrder)
              res4 <- updateOrderToDB(orderWithNewStatus)
            } yield (res1, res2, res3, res4)
            val result = Await.result(update, Duration.Inf)
            result match {
              case (SUCCESS, SUCCESS, SUCCESS, SUCCESS) =>
                // add a new trade record to database
                val trade = Trade(0, new Timestamp(System.currentTimeMillis()), midPrice, amount, total,
                  order.tradetype, trader.id, tradee.id, order.id, orderTradeWith.id)
                tradeRepo.add(trade)
              case (_, _, _, _) => Future.successful(FAIL)
            }
            /*
          update.flatMap{
            case (SUCCESS, SUCCESS, SUCCESS, SUCCESS) =>
              // add a new trade record to database
              val trade = Trade(0, new Timestamp(System.currentTimeMillis()), midPrice, amount, total,
                order.tradetype,trader.id, tradee.id)
              tradeRepo.add(trade)
            case (_, _, _, _) => Future.successful(FAIL)
          }
          */
          } else { // orderTradeWith.outstanding > order.outstanding
            midPrice = Calculator.midprice(order.price, orderTradeWith.price) // mid price
            diffPrice = Calculator.sub(order.price, midPrice).abs
            amount = order.outstanding
            total = Calculator.toFixed(Calculator.multi(midPrice, amount))
            refund = Calculator.toFixed(Calculator.multi(diffPrice, amount))

            // update user
            val traderGBPB = Calculator.add(trader.gbpBalance, refund)
            val traderGBPR = Calculator.sub(trader.gbpReserved, Calculator.add(total, refund))
            val tradeeGBPB = Calculator.add(tradee.gbpBalance, total)
            val traderBTCB = Calculator.add(trader.btcBalance, amount)
            val tradeeBTCR = Calculator.sub(tradee.btcReserved, amount)

            val traderUpdate =
              if (trader.id == tradee.id)
                trader.copy(gbpBalance = Calculator.add(tradeeGBPB, refund), gbpReserved = traderGBPR,
                  btcBalance = traderBTCB, btcReserved = tradeeBTCR)
              else
                trader.copy(gbpBalance = traderGBPB, gbpReserved = traderGBPR, btcBalance = traderBTCB)
            val tradeeUpdate =
              if (trader.id == tradee.id)
                traderUpdate
              else
                tradee.copy(gbpBalance = tradeeGBPB, btcReserved = tradeeBTCR)

            //update orders
            val tradedOrder = orderTradeWith.copy(outstanding = Calculator.sub(orderTradeWith.outstanding, order.outstanding))
            val orderWithNewStatus = order.copy(outstanding = 0.0, status = STATUS_CLOSED)
            val update = for {
            //update to database
              res1 <- userRepo.update(traderUpdate)
              res2 <- userRepo.update(tradeeUpdate)
              res3 <- updateOrderToDB(tradedOrder)
              res4 <- updateOrderToDB(orderWithNewStatus)
            } yield (res1, res2, res3, res4)
            val result = Await.result(update, Duration.Inf)
            result match {
              case (SUCCESS, SUCCESS, SUCCESS, SUCCESS) =>
                // add a new trade record to database
                val trade = Trade(0, new Timestamp(System.currentTimeMillis()), midPrice, amount, total,
                  order.tradetype, trader.id, tradee.id, order.id, orderTradeWith.id)
                tradeRepo.add(trade)
              case (_, _, _, _) => Future.successful(FAIL)
            }
          }
        case SELL =>
          if (order.outstanding >= orderTradeWith.outstanding) {
            midPrice = Calculator.midprice(order.price, orderTradeWith.price) // mid price
            diffPrice = Calculator.sub(order.price, midPrice).abs
            amount = orderTradeWith.outstanding
            total = Calculator.toFixed(Calculator.multi(midPrice, amount))
            refund = Calculator.toFixed(Calculator.multi(diffPrice, amount))

            // update user
            val traderGBPB = Calculator.add(trader.gbpBalance, total)
            val traderBTCR = Calculator.sub(trader.btcReserved, amount)
            val tradeeGBPR = Calculator.sub(tradee.gbpReserved, Calculator.add(total, refund))
            val tradeeGBPB = Calculator.add(tradee.gbpBalance, refund)
            val tradeeBTCB = Calculator.add(tradee.btcBalance, amount)

            val traderUpdate =
              if (trader.id == tradee.id)
                trader.copy(gbpBalance = Calculator.add(traderGBPB, refund), gbpReserved = tradeeGBPR,
                  btcBalance = tradeeBTCB, btcReserved = traderBTCR)
              else
                trader.copy(gbpBalance = traderGBPB, btcReserved = traderBTCR)
            val tradeeUpdate =
              if (trader.id == tradee.id)
                traderUpdate
              else
                tradee.copy(gbpBalance = tradeeGBPB, gbpReserved = tradeeGBPR, btcBalance = tradeeBTCB)

            // update order
            val tradedOrder = orderTradeWith.copy(outstanding = 0.0, status = STATUS_CLOSED)
            val orderWithNewStatus = Calculator.sub(order.outstanding, amount) match {
              case 0.0 => order.copy(outstanding = 0.0, status = STATUS_CLOSED)
              case left => order.copy(outstanding = left)
            }
            val update = for {
            //update to database
              res1 <- userRepo.update(traderUpdate)
              res2 <- userRepo.update(tradeeUpdate)
              res3 <- updateOrderToDB(tradedOrder)
              res4 <- updateOrderToDB(orderWithNewStatus)
            } yield (res1, res2, res3, res4)
            val result = Await.result(update, Duration.Inf)
            result match {
              case (SUCCESS, SUCCESS, SUCCESS, SUCCESS) =>
                // add a new trade record to database
                val trade = Trade(0, new Timestamp(System.currentTimeMillis()), midPrice, amount, total,
                  order.tradetype, trader.id, tradee.id, order.id, orderTradeWith.id)
                tradeRepo.add(trade)
              case (_, _, _, _) => Future.successful(FAIL)
            }
          } else { // orderTradeWith.outstanding > order.outstanding
            midPrice = Calculator.midprice(order.price, orderTradeWith.price) // mid price
            diffPrice = Calculator.sub(order.price, midPrice).abs
            amount = order.outstanding
            total = Calculator.toFixed(Calculator.multi(midPrice, amount))
            refund = Calculator.toFixed(Calculator.multi(diffPrice, amount))

            // update user
            val traderGBPB = Calculator.add(trader.gbpBalance, total)
            val traderBTCR = Calculator.sub(trader.btcReserved, amount)
            val tradeeGBPR = Calculator.sub(tradee.gbpReserved, Calculator.add(total, refund))
            val tradeeGBPB = Calculator.add(tradee.gbpBalance, refund)
            val tradeeBTCB = Calculator.add(tradee.btcBalance, amount)

            val traderUpdate =
              if (trader.id == tradee.id)
                trader.copy(gbpBalance = Calculator.add(traderGBPB, refund), gbpReserved = tradeeGBPR,
                  btcBalance = tradeeBTCB, btcReserved = traderBTCR)
              else
                trader.copy(gbpBalance = traderGBPB, btcReserved = traderBTCR)
            val tradeeUpdate =
              if (trader.id == tradee.id)
                traderUpdate
              else
                tradee.copy(gbpBalance = tradeeGBPB, gbpReserved = tradeeGBPR, btcBalance = tradeeBTCB)

            //update orders
            val tradedOrder = orderTradeWith.copy(outstanding = Calculator.sub(orderTradeWith.outstanding, order.outstanding))
            val orderWithNewStatus = order.copy(outstanding = 0.0, status = STATUS_CLOSED)
            val update = for {
            //update to database
              res1 <- userRepo.update(traderUpdate)
              res2 <- userRepo.update(tradeeUpdate)
              res3 <- updateOrderToDB(tradedOrder)
              res4 <- updateOrderToDB(orderWithNewStatus)
            } yield (res1, res2, res3, res4)
            val result = Await.result(update, Duration.Inf)
            result match {
              case (SUCCESS, SUCCESS, SUCCESS, SUCCESS) =>
                // add a new trade record to database
                val trade = Trade(0, new Timestamp(System.currentTimeMillis()), midPrice, amount, total,
                  order.tradetype, trader.id, tradee.id, order.id, orderTradeWith.id)
                tradeRepo.add(trade)
              case (_, _, _, _) => Future.successful(FAIL)
            }
          }
      }

  }


  // Market Order functions
  /**
    * find enough orders to deal with a market order
    * @param morder new market order
    * @return
    */
  def findEnoughOrders(morder: Order): Future[Seq[Order]] = {
    findEnoughOrders(morder.tradetype, morder.amount)
  }

  def findEnoughOrders(tradeType: String, amount: Double): Future[Seq[Order]] ={
    var tempAmount = 0.0
    var i = 0
    var x: Seq[Order] = Seq()
    tradeType match{
      case BUY =>
        val query = orders.filter(r => r.status===STATUS_OPEN && r.tradetype=== SELL)
          .sortBy(r => (r.price.asc, r.timestamp))
        db.run(query.result).map{
          list =>
            while(tempAmount < amount && i < list.length){
              tempAmount += list(i).outstanding
              x = x :+ list(i)
              i += 1
            }
            x
        }
      case SELL =>
        val query = orders.filter(r => r.status===STATUS_OPEN && r.tradetype=== BUY)
          .sortBy(r => (r.price.desc, r.timestamp))
        db.run(query.result).map{
          list =>
            while(tempAmount < amount && i < list.length){
              tempAmount += list(i).outstanding
              x = x :+ list(i)
              i += 1
            }
            x
        }
    }
  }

  def estimateEarn(tradeType: String, amount: Double):Future[Double] = {
    var outstanding = amount
    var sum = 0.0
    // get enough orders
    findEnoughOrders(tradeType, amount).map{
      list =>
        if(list.nonEmpty){
          // calculate sum
          if(list.length == 1){
            Calculator.toFixed(Calculator.multi(list.head.price, Math.min(amount, list.head.outstanding)))
          } else{
            for(i <- 0 to list.length-2){
              outstanding -= list(i).outstanding
              sum += (list(i).price * list(i).outstanding)
            }
            sum += list.last.price * Math.min(outstanding, list.last.outstanding)
            Calculator.toFixed(sum)
          }
        }else
          0.0
    }
  }

  /**
    * new market order
    * @param morder
    */
  def newMarketOrder(morder: Order): Future[Int] = {
    var code = SUCCESS
    // first add order to order book
    addOrderToDB(morder).flatMap{
      case 0L => Future.successful(FAIL)
      case id =>
        val morderWithId = morder.copy(id = id)
        findEnoughOrders(morderWithId).flatMap{
          list =>
            list.foreach{
              order =>
                val update =
                  for{
                    trader <- userRepo.findById(morder.uid)
                    tradee <- userRepo.findById(order.uid)
                    mOrder <- findById(morderWithId.id)
                    res <- marketOrderTrade(morderWithId, order, trader.get, tradee.get)
                  } yield res
                code = Await.result(update, Duration.Inf)
            }
            Future.successful(code)
        }
    }
  }

  def marketOrderTrade(morder: Order, order: Order, trader: User, tradee: User): Future[Int] = {
    val os =
      if(morder.outstanding > order.outstanding)
        Calculator.sub(morder.outstanding, order.outstanding)
      else
        Calculator.sub(order.outstanding, morder.outstanding)
    val amount = Math.min(morder.outstanding, order.outstanding)
    val sum = Calculator.toFixed(Calculator.multi(amount, order.price))
    // check balance
    morder.tradetype match{
      case BUY =>
        if(trader.gbpBalance >= sum){
            // update user account
            val traderGbpBalance = Calculator.sub(trader.gbpBalance, sum)
            val tradeeGbpBalance = Calculator.add(tradee.gbpBalance, sum)
            val traderBtcBalance = Calculator.add(trader.btcBalance, amount)
            val tradeeBtcReserved = Calculator.sub(tradee.btcReserved, amount)

            val traderUpdate =
              if(trader.id == tradee.id)
                trader.copy(btcBalance = traderBtcBalance, btcReserved = tradeeBtcReserved)
              else
                trader.copy(gbpBalance = traderGbpBalance, btcBalance = traderBtcBalance)

            val tradeeUpdate =
              if(trader.id == tradee.id)
                traderUpdate
              else
                tradee.copy(gbpBalance = tradeeGbpBalance, btcReserved = tradeeBtcReserved)

            // update order
            val tradedOrder =
              if(morder.outstanding > order.outstanding)
                order.copy(outstanding = 0.0, status = STATUS_CLOSED)
              else
                order.copy(outstanding = os)

            val morderChanged =
              if(morder.outstanding > order.outstanding)
                morder.copy(outstanding = os)
              else
                morder.copy(outstanding = 0.0)

          // Update to DB
          val update = for {
            res1 <- userRepo.update(traderUpdate)
            res2 <- userRepo.update(tradeeUpdate)
            res3 <- updateOrderToDB(tradedOrder)
            res4 <- updateOrderToDB(morderChanged)
          } yield (res1, res2, res3, res4)
          update.flatMap{
            case (SUCCESS, SUCCESS, SUCCESS, SUCCESS) =>
              // success, add a trade log
              val trade = Trade(0, new Timestamp(System.currentTimeMillis()), order.price, amount, sum,
                order.tradetype,trader.id, tradee.id, morder.id, order.id)
              tradeRepo.add(trade)
            case (_, _, _, _) => Future.successful(FAIL)
          }
        } else
          Future.successful(NOT_ENOUGH_GBP_BALANCE)
      case SELL =>
        if(trader.btcBalance >= amount){
          val tradeeGbpReserved = Calculator.sub(tradee.gbpReserved, sum)
          val traderGbpBalance = Calculator.add(trader.gbpBalance, sum)
          val tradeeBtcBalance = Calculator.add(tradee.btcBalance, amount)
          val traderBtcBalance = Calculator.sub(trader.btcBalance, amount)

          val traderUpdate =
            if(trader.id == tradee.id)
              trader.copy(gbpBalance = traderBtcBalance, gbpReserved = tradeeGbpReserved)
            else
              trader.copy(gbpBalance = traderGbpBalance, btcBalance = traderBtcBalance)
          val tradeeUpdate =
            if(trader.id == tradee.id)
              traderUpdate
            else
              tradee.copy(gbpReserved = tradeeGbpReserved, btcBalance = tradeeBtcBalance)

          // update order
          val tradedOrder =
            if(morder.outstanding > order.outstanding)
              order.copy(outstanding = 0.0, status = STATUS_CLOSED)
            else
              order.copy(outstanding = os)

          val morderChanged =
            if(morder.outstanding > order.outstanding)
              morder.copy(outstanding = os)
            else
              morder.copy(outstanding = 0.0)

          // Update to DB
          val update = for {
            res1 <- userRepo.update(traderUpdate)
            res2 <- userRepo.update(tradeeUpdate)
            res3 <- updateOrderToDB(tradedOrder)
            res4 <- updateOrderToDB(morderChanged)
          } yield (res1, res2, res3, res4)
          update.flatMap{
            case (SUCCESS, SUCCESS, SUCCESS, SUCCESS) =>
              // success, add a trade log
              val trade = Trade(0, new Timestamp(System.currentTimeMillis()), order.price, amount, sum,
                order.tradetype,trader.id, tradee.id, morder.id, order.id)
              tradeRepo.add(trade)
            case (_, _, _, _) => Future.successful(FAIL)
          }
        } else
          Future.successful(NOT_ENOUGH_BTC_BALANCE)
    }
  }
}

