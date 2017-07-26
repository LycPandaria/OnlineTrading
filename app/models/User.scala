package models

/**
  * Created by lyc08 on 2017/6/27.
  */
case class User(id: Long, username: String, password: String, email: String,
                btcBalance: Double, gbpBalance: Double, btcReserved: Double, gbpReserved: Double)
case class LoginForm(username: String, password: String)
case class RegisterForm(username: String, password: String, email: Option[String])

