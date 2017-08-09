package models

import java.sql.Timestamp

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}




/**
  * Created by lyc08 on 2017/7/5.
  */
case class Trade(id: Long, timestamp: Timestamp, tradePrice: Double, amount: Double, total: Double, tradeType: String,
                 traderId: Long, tradeeId: Long, orderId: Long, orderTradeWithId: Long)
object Trade {
  implicit val TradeWrites: Writes[Trade] = (
    (JsPath \ "id").write[Long] and
      (JsPath \ "ts").write[Timestamp] and
      (JsPath \ "tradePrice").write[Double] and
      (JsPath \ "amount").write[Double] and
      (JsPath \ "total").write[Double] and
      (JsPath \ "tradeType").write[String] and
      (JsPath \ "traderId").write[Long] and
      (JsPath \ "tradeeId").write[Long] and
      (JsPath \ "orderId").write[Long] and
      (JsPath \ "orderTradeWithId").write[Long]
    )(unlift(Trade.unapply))
}

