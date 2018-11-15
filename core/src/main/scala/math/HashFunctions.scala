package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.collection.mutable
import scala.util.Random

trait HashingArgs extends Serializable
/**
 * Trait regrouping various hashing methods
 */
trait Hasher[V] extends Serializable {
  def hf(v: V): Double
}
/**
 * A basic implementation of Locality Sensitive Hashing
 */
class LSH[V <: Seq[Double]](val dim: Int, val w: Double = 1D) extends Hasher[V] {

  val b = Random.nextDouble * w
  /**
   * Create a random vector where component are taken on normal law N(0,1) for LSH
   */
  val hashVector = mutable.ArrayBuffer.fill(dim)(Random.nextGaussian)
  /**
   *  Generate the hash value for a given vector x depending on w, b, hashVector
   */
  def hf(x: V): Double = {
    @annotation.tailrec
    def go(s: Double, i: Int): Double = {
      if( i < x.size ) go(s + x(i) * hashVector(i), i + 1)
      else s
    }
    (go(0D, 0) + b) / w
  }
}

class EasyLSH(dim: Int, w: Double = 1D) extends LSH[mutable.ArrayBuffer[Double]](dim, w)