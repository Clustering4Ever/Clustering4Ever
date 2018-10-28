package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.reflect.ClassTag
import scala.math.sqrt
import scala.collection.GenSeq
import scala.language.higherKinds
import spire.math.{Numeric => SNumeric}
/**
 * Object which gather common operation on Seq[N] and GenSeq[Seq[N]]
 */
object SumVectors {
	/**
	 * Sum two vector of Numeric into one
	 */
	def sumVectors[@specialized(Int, Double) N, V[N] <: Seq[N]](a: V[N], b: V[N])(implicit num: SNumeric[N]): V[N] = a.zip(b).map{ case (c, d) => num.plus(c, d) }.asInstanceOf[V[N]]
	/**
	 * Reduce an Array[Array[N]] into an Array[N]
	 */
	def sumColumnMatrix[@specialized(Int, Double) N: SNumeric, V[N] <: Seq[N]](cluster: GenSeq[V[N]]): V[N] = cluster.reduce(sumVectors(_, _))
	/**
	 * Reduce Array of multiple vectors
	 */
	def reduceMultipleVectorsMatrice[V[Double] <: Seq[Double], It <: Seq[V[Double]]](a: It, b: It) = a.zip(b).map{ case (c, d) => sumVectors(c, d) }.asInstanceOf[It]
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