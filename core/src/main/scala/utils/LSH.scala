package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.collection.mutable
import scala.util.Random

trait HashingArgs extends Serializable
/**
 * Trait regrouping various hashing methods
 */
trait Hasher[V <: Seq[Double]] extends Serializable {
  def hf(v: V): Double
}
/**
 * A basic implementation of Locality Sensitive Hashing
 */
class LSH(val dim: Int, val w: Double = 1D) extends Hasher[mutable.ArrayBuffer[Double]] {

  val b = Random.nextDouble * w
  /**
   * Create a random vector where component are taken on normal law N(0,1) for LSH
   */
  val hashVector = mutable.ArrayBuffer.fill(dim)(Random.nextGaussian)
  /**
   *  Generate the hash value for a given vector x depending on w, b, hashVector
   */
  def hf(x: mutable.ArrayBuffer[Double]): Double = {
  	var s = 0D
    var i = 0
    while( i < x.size ) {
      s += x(i) * hashVector(i)
      i += 1
    }
  	( s + b ) / w
  }

}