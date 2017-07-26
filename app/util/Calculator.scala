package util

/**
  * Created by lyc08 on 2017/6/29.
  */
object Calculator {
  import scala.math.BigDecimal
  def add(x:Double, y:Double): Double = {
    (BigDecimal(x.toString) + BigDecimal(y.toString)).toString().toDouble
  }

  def sub(x:Double, y:Double): Double = {
    (BigDecimal(x.toString) - BigDecimal(y.toString)).toString().toDouble
  }

  def midprice(x:Double, y:Double):Double = {
    ((BigDecimal(x.toString) + BigDecimal(y.toString)) / 2).toString().toDouble
  }

  def multi(x:Double, y: Double):Double = {
    (BigDecimal(x.toString) * BigDecimal(y.toString)).toString().toDouble
  }

  def toFixed(x: Double): Double = {
    x.formatted("%.2f").toDouble
  }

}
