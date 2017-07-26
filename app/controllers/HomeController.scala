package controllers

import java.sql.Timestamp
import javax.inject._

import dal.{OrderRepository, UserRepository}
import models.Order
import play.api._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._

import scala.concurrent.ExecutionContext

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(repo: OrderRepository, userRepo: UserRepository, val messagesApi: MessagesApi)(implicit ec:ExecutionContext)
  extends Controller with I18nSupport{

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index = Action { implicit request =>

    Ok(views.html.index())
  }
}
