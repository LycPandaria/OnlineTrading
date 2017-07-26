package controllers

import play.api.mvc._
import javax.inject._

import dal.{OrderRepository, TradeRepository}
import models.{Trade, User}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.{JsPath, JsValue, Json, Writes}
import play.api.libs.functional.syntax._
import util.AuthUtil

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
/**
  * Created by lyc08 on 2017/7/5.
  */
@Singleton
class TradeController @Inject()(authUtil: AuthUtil,val messagesApi: MessagesApi,tradeRepo: TradeRepository, orderRepo: OrderRepository)
                               (implicit ec:ExecutionContext)
  extends Controller with I18nSupport {



  def lastPrice = Action.async {
    tradeRepo.getLastPrice().map {
      lastPrice =>
        Ok(Json.toJson(lastPrice))
    }
  }

  def userTrades = Authenticated{
    user =>
      implicit request=>
      tradeRepo.findTradesByUid(user.id).map{
        trades =>
          Ok(Json.toJson(trades))
      }
  }

  def ajaxTicker = Action.async {
    val ticker =
      for {
        lastPrice <- tradeRepo.getLastPrice()
        bid <- orderRepo.maxBuyPrice()
        ask <- orderRepo.minSellPrice()
      } yield (lastPrice, bid, ask)
    ticker.map{
      case (lastPrice, bid, ask) =>
        val json:JsValue = Json.obj("lastPrice" -> lastPrice, "bid" -> bid, "ask" -> ask)
        Ok(json)
    }
  }

  def ajaxTrades = Action.async{
    tradeRepo.list().map{
      trades =>
        Ok(Json.toJson(trades))
    }
  }

  // Authentication
  def Authenticated(f: => User => Request[AnyContent] => Future[Result]) = {
    Action.async {
      implicit request =>
        // can only use flatMap
        authUtil.parseUserFromCookie.flatMap {
          case Some(user) => f(user)(request)
          case None => Future.successful(
            Redirect(routes.UserController.loginPage()).flashing("error" -> "You have to login in.")
          )
        }
    }
  }


}
