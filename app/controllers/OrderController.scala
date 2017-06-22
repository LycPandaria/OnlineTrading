package controllers

/**
  * Created by lyc08 on 2017/6/18.
  */
import java.sql.Timestamp

import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms.{longNumber, mapping, nonEmptyText}
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.Flash

import scala.concurrent.{ExecutionContext, Future}
import play.api.data.format.Formats._
import play.api.data._
import play.api.data.Forms._
import javax.inject._

import dal.OrderRepository
import models.{Order, OrderForm}

@Singleton
class OrderController @Inject()(repo: OrderRepository, val messagesApi: MessagesApi) (implicit ec:ExecutionContext)
  extends Controller with I18nSupport{

  val STATUS_OPEN = "open"
  val TRADETYPE_SELL = "sell"
  val TRADETYPE_BUY = "buy"

  /*
  get all the orders in two Seq[OrderForm]--buys and sells
   */
  def list = Action.async{ implicit request =>
    val all = for {
      // get two Seq[OrderForm], contain (tradetype, price, total_amount)
      // total_amount is a sum of amount in different orders with same price
      buys <- repo.listCombinedOrders(TRADETYPE_BUY)
      sells <- repo.listCombinedOrders(TRADETYPE_SELL)
    } yield (buys, sells)

    all.map{
      case (buys, sells) =>
        Ok(views.html.orders.list(buys, sells))
    }
  }

  def newOrder = Action{ implicit request =>
    val form =
      if(request.flash.get("error").isDefined)
        // if there is error in form, bind flash scope data to form
        orderForm.bind(request.flash.data)
      else
        orderForm
    Ok(views.html.orders.edit(form))
  }

  def save = Action {implicit request =>
    // fill the form using user input
    val newOrderForm = orderForm.bindFromRequest()
    newOrderForm.fold(
      // if validation fail, redirect to add page
      hasErrors = {
        form =>
          Redirect(routes.OrderController.newOrder())
            .flashing(Flash(form.data) + ("error" -> Messages("validation.errors")))
      },
      success = { newOrderForm =>
        val order = Order(0,new Timestamp(System.currentTimeMillis()), newOrderForm.tradetype,
          newOrderForm.price, newOrderForm.amount, newOrderForm.amount,STATUS_OPEN)
        // add to DB
        repo.add(order)
        val message = Messages("orders.new.success")
        Redirect(routes.OrderController.list()).flashing("success" -> message)
      }
    )
  }

  private val orderForm = Form(
    mapping(
      "tradetype" -> nonEmptyText,
      "price" -> of(doubleFormat),
      "amount" -> of(doubleFormat)
    )(OrderForm.apply)(OrderForm.unapply)
  )




}
