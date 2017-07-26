package controllers

/**
  * Created by lyc08 on 2017/6/18.
  */
import java.sql.Timestamp

import play.api.mvc._
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.Flash

import scala.concurrent.{ExecutionContext, Future}
import javax.inject._

import dal.{OrderRepository, TradeRepository, UserRepository}
import models.{Order, OrderForm, User}
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText, of}
import play.api.data.format.Formats.doubleFormat
import play.api.libs.json.Json
import util.AuthUtil

@Singleton
class OrderController @Inject()(repo: OrderRepository, userRepo: UserRepository, tradeRepo: TradeRepository,
                                val messagesApi: MessagesApi, authUtil: AuthUtil)
                               (implicit ec:ExecutionContext)
  extends Controller with I18nSupport{

  val LIMIT = "limit"
  val MARKET = "market"
  val STATUS_OPEN = "open"
  val TRADETYPE_SELL = "sell"
  val TRADETYPE_BUY = "buy"
  val NO_SUCH_USER = 2
  val SUCCESS = 1
  val NOT_ENOUGH_GBP_BALANCE = 0
  val NOT_ENOUGH_BTC_BALANCE = 3
  val FAIL = -1

  /*
  get all the orders in two Seq[OrderForm]--buys and sells
   */
  def list = Action.async{ implicit request =>
    val all = for {
      // get two Seq[OrderForm], contain (tradetype, price, total_amount)
      // total_amount is a sum of amount in different orders with same price
      buys <- repo.listCombinedOrders(TRADETYPE_BUY)
      sells <- repo.listCombinedOrders(TRADETYPE_SELL)
      trades <- tradeRepo.list()
    } yield (buys, sells, trades)

    all.map{
      case (buys, sells, trades) =>
        Ok(views.html.orders.list(buys, sells, trades))
    }
  }

  def ajaxTickers = Action.async{
    val tickers = for{
      buys <- repo.listCombinedOrders(TRADETYPE_BUY)
      sells <- repo.listCombinedOrders(TRADETYPE_SELL)
    } yield (buys, sells)
    tickers.map{
      case (buys, sells) =>
        val json = Json.obj(
          "buys" -> Json.toJson(buys),
          "sells" -> Json.toJson(sells)
        )
        Ok(json)
    }
  }

  def userOrders = Authenticated{
    user =>
      implicit request =>
      repo.findOrdersByUid(user.id).map{
        orders =>
          Ok(Json.toJson(orders));
      }
  }

  def newOrder = Authenticated{
    user =>
      implicit request =>
        val form =
          if(request.flash.get("error").isDefined)
          // if there is error in form, bind flash scope data to form
            orderForm.bind(request.flash.data)
          else
            orderForm
        Future.successful{
          Ok(views.html.orders.edit(form, user))
        }
  }

  def save = Authenticated {
    user =>
    implicit request =>
    // fill the form using user input
    val newOrderForm = orderForm.bindFromRequest()
    newOrderForm.fold(
      // if validation fail, redirect to add page
      hasErrors = {
        form =>
          Future.successful{
            Redirect(routes.OrderController.newOrder())
              .flashing(Flash(form.data) + ("error" -> Messages("validation.errors")))
          }
      },
      success = { newOrderForm =>
        val order = Order(0,new Timestamp(System.currentTimeMillis()), newOrderForm.tradetype,
          newOrderForm.price, newOrderForm.amount, newOrderForm.amount,STATUS_OPEN, user.id, LIMIT)
        // add to DB
        repo.add(order).map{
          case SUCCESS =>
            val message = Messages("orders.new.success")
            Redirect(routes.OrderController.newOrder()).flashing("success" -> message)
          case code =>
            println("ERROR:" + code)
            Redirect(routes.OrderController.newOrder()).flashing("error" -> "System error.")
        }
      }
    )
  }

  def newMarketOrderPage = Authenticated{
    user =>
      implicit request =>
      Future.successful(Ok(views.html.orders.marketorder(user)))
  }

  def addMarketOrder(tradeType: String, amount: Double) = Authenticated{
    user =>
      implicit request =>
      val marketOrder = Order(0,new Timestamp(System.currentTimeMillis()), tradeType, -1.0, amount, amount, "close",
        user.id, "market")
      repo.newMarketOrder(marketOrder).map{
        case SUCCESS =>
          val message = Messages("orders.new.success")
          Redirect("/orders/marketOrder").flashing("success" -> message)
        case NOT_ENOUGH_BTC_BALANCE =>
          val message = Messages("orders.not.enough.btc")
          Redirect("/orders/marketOrder").flashing("error" -> message)
        case NOT_ENOUGH_GBP_BALANCE =>
          val message = Messages("orders.not.enough.gbp")
          Redirect("/orders/marketOrder").flashing("error" -> message)
        case FAIL =>
          Redirect(routes.OrderController.newOrder()).flashing("error" -> "System error.")
      }
  }

  def estimateCost(tradeType: String, amount: Double) = Action.async{
    repo.estimateEarn(tradeType, amount).map{
      cost => Ok(Json.toJson(cost))
    }
  }

  // Authentication
  def Authenticated(f: => User => Request[AnyContent] => Future[Result]) = Action.async{
     implicit request =>
      authUtil.parseUserFromCookie.flatMap {
        case Some(user) => f(user)(request)
        case None => Future.successful(
          Redirect(routes.UserController.loginPage()).flashing("error" -> "You have to login in."))
      }
  }

  /**
    * API used to quick testing
    * @param uid
    * @param tradeType
    * @param amount
    * @param price
    */
  def apiNewLimitOrder(uid: Long, tradeType: String, amount: Double, price: Double) = Action.async{
    val order = Order(0,new Timestamp(System.currentTimeMillis()), tradeType, price, amount, amount,STATUS_OPEN, uid, LIMIT)
    repo.add(order).map{
      case SUCCESS => Ok("success")
      case error => Ok("fail")
    }
  }

  def apiNewMarketOrder(uid: Long, tradeType: String, amount: Double) = Action.async{
    val marketOrder = Order(0,new Timestamp(System.currentTimeMillis()), tradeType, -1.0, amount, amount, "close",
      uid, "market")
    repo.newMarketOrder(marketOrder).map{
      case SUCCESS => Ok("success")
      case error => Ok("fail")
    }
  }

  private val orderForm: Form[OrderForm] = Form(
    mapping(
      "tradetype" -> nonEmptyText,
      "price" -> of(doubleFormat),
      "amount" -> of(doubleFormat)
    )(OrderForm.apply)(OrderForm.unapply)
  )

}
