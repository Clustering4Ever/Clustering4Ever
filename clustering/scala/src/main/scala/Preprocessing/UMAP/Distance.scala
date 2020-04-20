package org.clustering4ever.scala.umap
/**
 * @author Beugnet Vincent
 * @author Hurvois Guillaume
 * @author Ladjal Adlane
 * @author Merien Grégoire
 * @author Serfas Florent
 * @author Beck Gaël
 * @author Forest Florent
 */
import breeze.linalg._
import breeze.numerics._

trait Distance {
    def apply(x: Array[Double], y: Array[Double]): Double
}

final case class Euclidean() extends Distance {
  def apply(x: Array[Double], y: Array[Double]): Double = {

    @annotation.tailrec
    def go(i: Int, result: Double): Double = {
      val diff = x(i) - y(i)
      val res = result + diff * diff
      if (i < x.length - 1 && x.length == y.length) {
        go(i + 1, res)
      }
      else sqrt(res)
    }
    go(0, 0)
  }
}

final case class Hamming() extends Distance {
  def apply(x: Array[Double], y: Array[Double]): Double = {

    @annotation.tailrec
    def go(i: Int, result: Double): Double = {
      val diff = x(i).toInt ^ y(i).toInt
      val res = result + diff
      if (i < x.length - 1 && x.length == y.length) {
        go(i + 1, res)
      }
      else res
    }
    go(0, 0D)
  }
}

final case class Minkowski(p: Int) extends Distance {

  def apply(x: Array[Double], y: Array[Double]): Double = {

    @annotation.tailrec
    def go(i: Int, result: Double): Double = {
      if (i < x.length && x.length == y.length) {
        go(i + 1, result + pow(abs(x(i) - y(i)), p)
        )
      }
      else result
    }
    go(0, 0)
  }
}
