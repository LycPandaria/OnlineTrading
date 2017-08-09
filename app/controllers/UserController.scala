package controllers

/**
  * Created by lyc08 on 2017/6/27.
  */
import play.api.mvc._
import javax.inject._

import dal.UserRepository
import models.{LoginForm, RegisterForm, User}
import util.AuthUtil
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.data.Form
import play.api.data.Forms._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}

@Singleton
class UserController @Inject()(val messagesApi: MessagesApi, repo: UserRepository, authUtil: AuthUtil)
                              (implicit ec:ExecutionContext) extends Controller with I18nSupport{

  val SUCCESS = 1
  val NOT_ENOUGH_GBP_BALANCE = 0
  val NOT_ENOUGH_BTC_BALANCE = 3

  private val loginForm: Form[LoginForm] = Form(
    mapping(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText
    )(LoginForm.apply)(LoginForm.unapply)
  )


  private val registerForm: Form[RegisterForm] = Form (
    mapping(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText,
      "email" -> optional(email)
    )(RegisterForm.apply)(RegisterForm.unapply)
  )

  def loginPage = Action { implicit request =>
    request.session.get("username").map{
      username =>
        // if logged, just redirect to list page with this user
        Redirect(routes.HomeController.index()).flashing("success" -> Messages("login.welcome", username))
    }.getOrElse{
      val form =
        if(request.flash.get("error").isDefined)
          loginForm.bind(request.flash.data)
        else
          loginForm
      Ok(views.html.users.login(form))
    }
  }

  def login = Action.async { implicit request =>
    // get form data
    loginForm.bindFromRequest().fold(
      hasErrors = {
        form=>
          Future.successful(Redirect(routes.UserController.loginPage()).flashing("errors" -> Messages("login.inputError")))
      },
      success = {
        logininfo=>
          repo.login(logininfo.username, logininfo.password).map{
            user =>
              if(user.isDefined){
                // success
                Redirect(routes.HomeController.index())
                  .withSession("username" -> user.get.username, "userid" -> user.get.id.toString)
                  .flashing("success" -> Messages("login.welcome", user.get.username))
              }else{
                // fail
                Redirect(routes.UserController.loginPage()).withNewSession
                  .flashing("error" -> Messages("login.error"))
              }
          }
      }
    )
  }

  // register page
  def registerPage = Action {
    implicit request =>
      val form =
        if(request.flash.get("error").isDefined)
          registerForm.bind(request.flash.data)
      else
          registerForm
      Ok(views.html.users.register(form))
  }

  // register user
  def register = Action.async{ implicit request=>
    val registerInfo = registerForm.bindFromRequest()
    registerInfo.fold(
      hasErrors = {
        errorForm =>
          Future.successful(
            Redirect(routes.UserController.registerPage())
              .flashing(Flash(errorForm.data) +
                ("error" -> Messages("register.validation.error")))
          )
      },
      success = {
        data =>
          repo.register(data.username, data.password, data.email.getOrElse("")).map{
            userid =>
              if(userid == 0L){
                // error -> duplicate username
                Redirect(routes.UserController.registerPage())
                  .flashing(Flash(registerInfo.data) +
                    ("error" -> Messages("register.validation.duplicate")))
              }else
                Redirect(routes.HomeController.index())
                  .flashing("success" -> Messages("login.welcome", data.username))
                  .withSession("username" -> data.username, "userid" -> userid.toString)
          }
      }
    )
  }

  // logout
  def logout = Action {implicit request =>
    Redirect(routes.HomeController.index()).withNewSession
  }

  def userOrdersPage = Authenticated{
    user =>
      implicit request =>
        Future.successful(Ok(views.html.orders.userorders()))
  }

  def userTradesPage = Authenticated{
    user =>
      implicit request =>
        Future.successful(Ok(views.html.orders.usertrades()))
  }

  def financeChannel = Authenticated{
    user =>
      implicit request =>
      Future.successful(Ok(views.html.users.financeindex(user)))
  }

  def gbpTopupPage = Authenticated{
    user =>
      implicit  request =>
      Future.successful(Ok(views.html.users.gbptopup()))
  }

  def btcTopupPage = Authenticated{
    user =>
      implicit request =>
      Future.successful(Ok(views.html.users.btctopup()))
  }

  def gbpTopup(amount: Double) = Authenticated{
    user =>
      implicit request =>
      repo.gbpTopup(amount, user).map{
        case SUCCESS =>
          Redirect(routes.UserController.financeChannel()).flashing("success" -> "Topup successfully!")
        case code =>
          val message = "Error: " + code
          Redirect(routes.UserController.gbpTopupPage()).flashing("error" -> message)
      }
  }

  def btcTopup(amount: Double) = Authenticated{
    user =>
      implicit request =>
      repo.btcTopup(amount, user).map{
        case SUCCESS =>
          Redirect(routes.UserController.financeChannel()).flashing("success" -> "Topup successfully!")
        case code =>
          val message = "Error: " + code
          Redirect(routes.UserController.btcTopupPage()).flashing("error" -> message)
      }
  }

  def gbpWithdrawalPage = Authenticated{
    user =>
      implicit request =>
      Future.successful(Ok(views.html.users.gbpwithdrawal(user)))
  }

  def btcWithdrawalPage = Authenticated{
    user =>
      implicit request =>
        Future.successful(Ok(views.html.users.btcwithdrawal(user)))
  }

  def gbpWithdrawal(amount: Double) = Authenticated{
    user =>
      implicit request =>
      repo.gbpWithdrawal(amount, user).map{
        case SUCCESS =>
          Redirect(routes.UserController.financeChannel()).flashing("success" -> Messages("withdrawal.success"))
        case NOT_ENOUGH_GBP_BALANCE =>
          Redirect(routes.UserController.gbpWithdrawalPage()).flashing("error" -> Messages("withdrawal.error.not.enough"))
        case code =>
          val message = "Error: " + code
          Redirect(routes.UserController.gbpWithdrawalPage()).flashing("error" -> message)
      }
  }

  def btcWithdrawal(amount: Double) = Authenticated{
    user =>
      implicit request =>
        repo.btcWithdrawal(amount, user).map{
          case SUCCESS =>
            Redirect(routes.UserController.financeChannel()).flashing("success" -> Messages("withdrawal.success"))
          case NOT_ENOUGH_GBP_BALANCE =>
            Redirect(routes.UserController.gbpWithdrawalPage()).flashing("error" -> Messages("withdrawal.error.not.enough"))
          case code =>
            val message = "Error: " + code
            Redirect(routes.UserController.gbpWithdrawalPage()).flashing("error" -> message)
        }
  }

  def updateTest(x: Double) = Action{
    blocking{
      this.synchronized {
        Ok(Await.result(repo.updateTest(x), Duration.Inf).toString)
      }
    }
  }

  // Authentication
  def Authenticated(f: => User => Request[AnyContent] => Future[Result]) = Action.async{
    implicit request =>
      authUtil.parseUserFromCookie.flatMap {
        case Some(user) => f(user)(request)
        case None => Future.successful(
          Redirect(routes.UserController.loginPage()).flashing("error" -> "You have to login in.")
        )
      }
  }


}
