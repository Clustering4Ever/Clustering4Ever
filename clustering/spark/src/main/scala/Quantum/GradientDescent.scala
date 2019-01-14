package Quantum
import math._

object GradientDescent {

  val curX = 6
  val gamma = 0.01
  val precision = 0.00001
  val previousStepSize = 1 / precision

  def df(x: Double): Double = 4 * pow(x, 3) - 9 * pow(x, 2)

  def gradientDescent(precision: Double, previousStepSize: Double, curX: Double): Double = {
    if (previousStepSize > precision) {
      val newX = curX + -gamma * df(curX)
      println(curX)
      gradientDescent(precision, abs(newX - curX), newX)
    } else curX
  }

  val ans = gradientDescent(precision, previousStepSize, curX)
  println(s"The local minimum occurs at $ans")
}
