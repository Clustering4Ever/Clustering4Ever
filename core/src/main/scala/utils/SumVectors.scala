package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.reflect.ClassTag
import scala.math.sqrt
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}
/**
 * Object which gather common operation on Seq[N] and GenSeq[Seq[N]]
 */
object SumVectors {
	/**
	 * Sum two vector of Numeric into one
	 */
	def sumVectors[@specialized(Int, Double) N, V <: Seq[N]](a: V, b: V)(implicit num: SNumeric[N]): V = a.zip(b).map{ case (c, d) => num.plus(c, d) }.asInstanceOf[V]
	/**
	 * Reduce an Array[Array[N]] into an Array[N]
	 */
	def sumColumnMatrix[@specialized(Int, Double) N: SNumeric, V <: Seq[N]](cluster: GenSeq[V]): V = cluster.reduce(sumVectors[N, V](_, _))
	/**
	 * Reduce Array of multiple vectors
	 */
	def reduceMultipleVectorsMatrice[V <: Seq[Double], It <: Seq[V]](a: It, b: It) = a.zip(b).map{ case (c, d) => sumVectors[Double, V](c, d) }.asInstanceOf[It]
	/**
	 *
	 */
	def norm[V <: Seq[Double]](dot: V): Double = {
	  var s = 0D
	  var i = 0
	  while( i < dot.size ) {
	    val v = dot(i)
	    s += v * v
	    i += 1
	  }
	  sqrt(s)
	}
	/**
	 *
	 */
	def dotProd[V <: Seq[Double]](dot1: V, dot2: V): Double = {
		var dp = 0D
		var i = 0
		while( i < dot1.size ) {
			dp += dot1(i) * dot2(i)
			i += 1
		}
		dp
	}
}