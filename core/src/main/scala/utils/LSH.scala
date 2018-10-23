package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.collection.mutable
/**
 * A basic implementation of Locality Sensitive Hashing
 */
object LSH {
  /**
   * Create a random vector where component are taken on normal law N(0,1) for LSH
   */
  def obtainHashVector(dim: Int): mutable.ArrayBuffer[Double] = mutable.ArrayBuffer.fill(dim)(scala.util.Random.nextGaussian)
  /**
   *  Generate the hash value for a given vector x depending on w, b, hashVector
   */
  def hf(x: mutable.ArrayBuffer[Double], w: Double, b: Double, hv: mutable.ArrayBuffer[Double]): Double = {
  	var s = 0D
    var i = 0
    while( i < x.size ) {
      s += x(i) * hv(i)
      i += 1
    }
  	( s + b ) / w
  }
}