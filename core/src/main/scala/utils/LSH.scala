package clustering4ever.util

import scala.collection.mutable

/**
 * @author Beck Gaël
 *
 *
 **/
object LSH
{
  /**
   * Create a random vector where component are taken on normal law N(0,1) for LSH
   */
  def obtainHashVector(dim: Int): mutable.ArrayBuffer[Double] = mutable.ArrayBuffer.fill(dim)(scala.util.Random.nextGaussian)
  /**
   *  Generate the hash value for a given vector x depending on w, b, hashVector
   */
  def hf(x: mutable.ArrayBuffer[Double], w: Double, b: Double, hv: mutable.ArrayBuffer[Double]): Double =
  {
  	var s = 0D
  	x.indices.foreach( i => s += x(i) * hv(i) )
  	( s + b ) / w
  }
}