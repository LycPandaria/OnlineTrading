package models

/**
  * Created by lyc08 on 2017/6/18.
  */
import java.sql.Timestamp

import play.api.libs.json.Json

case class Order(id: Long, timestamp: Timestamp, tradetype: String, price: Double, amount: Double, outstanding: Double,
                 status: String)
case class OrderForm(tradetype: String, price: Double, amount: Double)
object Order{
  //implicit val OrderFormat = Json.format[Order]


}


