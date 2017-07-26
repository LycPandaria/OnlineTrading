package util

import play.api.mvc._
import javax.inject._

import dal.UserRepository
import models.User

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by lyc08 on 2017/7/3.
  */
@Singleton
class AuthUtil @Inject()(userRepo: UserRepository) (implicit ec:ExecutionContext){

  def parseUserFromCookie(implicit request: RequestHeader): Future[Option[User]] = {
    val username = request.session.get("username")
    if(username.isDefined){
      userRepo.findByUsername(username.get)
    }else
      Future.successful(None)
  }

  case class AuthenticatedRequest[A](user: User, request: Request[A]) extends WrappedRequest[A](request)

  object AuthenticatedAction extends ActionBuilder[AuthenticatedRequest] {
    def invokeBlock[A](request: Request[A], block: (AuthenticatedRequest[A]) => Future[Result]) = {
      request.session.get("username") match {
        case Some(username) =>
          userRepo.findByUsername(username).flatMap{
            case Some(user) =>
              block(AuthenticatedRequest(user, request))
          }
      }
    }
  }



}
